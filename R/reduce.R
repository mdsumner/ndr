#' Reductions along named dimensions
#'
#' Summarise a Variable or DataArray by applying a function along one or
#' more named dimensions. The reduced dimensions are dropped from the result.
#'
#' @param x A Variable or DataArray
#' @param dims Character vector of dimension names to reduce over
#' @param na.rm Logical, whether to remove NAs
#' @return Same type as input, with reduced dimensions dropped
#'
#' @examples
#' temp <- Variable(
#'   dims = c("time", "lat", "lon"),
#'   data = array(rnorm(10 * 3 * 4), c(10, 3, 4))
#' )
#'
#' # Time mean
#' nd_mean(temp, "time")  # shape: lat=3, lon=4
#'
#' # Spatial mean (reduce lat and lon)
#' nd_mean(temp, c("lat", "lon"))  # shape: time=10
#'
#' # Global mean (reduce everything)
#' nd_mean(temp, c("time", "lat", "lon"))  # scalar
#'
#' @name reductions
NULL


#' @rdname reductions
#' @export
nd_mean <- new_generic("nd_mean", "x", function(x, dims, na.rm = FALSE) S7_dispatch())

#' @rdname reductions
#' @export
nd_sum <- new_generic("nd_sum", "x", function(x, dims, na.rm = FALSE) S7_dispatch())

#' @rdname reductions
#' @export
nd_min <- new_generic("nd_min", "x", function(x, dims, na.rm = FALSE) S7_dispatch())

#' @rdname reductions
#' @export
nd_max <- new_generic("nd_max", "x", function(x, dims, na.rm = FALSE) S7_dispatch())


# --- Variable methods ---

reduce_variable <- function(x, dims, fn, na.rm = FALSE) {
  fn_name <- fn
  fn <- match.fun(fn)
  s <- shape(x)
  all_dims <- names(s)

  bad <- setdiff(dims, all_dims)
  if (length(bad) > 0L) {
    stop(sprintf(
      "reduction dims not found: %s (available: %s)",
      paste(bad, collapse = ", "), paste(all_dims, collapse = ", ")
    ))
  }

  # which axes (integer positions) to KEEP (the MARGIN for apply)
  keep_axes <- which(!all_dims %in% dims)
  new_dims <- all_dims[keep_axes]
  arr <- var_data(x)

  # lazy data: stream chunk-aligned blocks instead of materialising
  if (is_lazy(arr)) {
    val <- reduce_lazy(arr, keep_axes, fn_name, na.rm = na.rm)
    if (length(keep_axes) == 0L) {
      return(Variable(dims = character(), data = array(val), attrs = x@attrs))
    }
    return(Variable(dims = new_dims, data = val, attrs = x@attrs))
  }

  if (length(keep_axes) == 0L) {
    # reducing all dims -> scalar
    val <- fn(arr, na.rm = na.rm)
    return(Variable(dims = character(), data = array(val), attrs = x@attrs))
  }

  # apply over kept margins
  result <- apply(arr, keep_axes, fn, na.rm = na.rm)

  # apply can return a vector when MARGIN is length 1 - ensure dim is set
  expected_shape <- unname(s[new_dims])
  if (is.null(dim(result))) {
    dim(result) <- expected_shape
  } else if (!identical(unname(dim(result)), unname(expected_shape))) {
    # apply may have transposed (it puts MARGIN dims in order of MARGIN)
    # our keep_axes are already in ascending order so this should be fine,
    # but let's be safe
    dim(result) <- expected_shape
  }

  Variable(dims = new_dims, data = result, attrs = x@attrs)
}


method(nd_mean, Variable) <- function(x, dims, na.rm = FALSE) {
  reduce_variable(x, dims, "mean", na.rm = na.rm)
}

method(nd_sum, Variable) <- function(x, dims, na.rm = FALSE) {
  reduce_variable(x, dims, "sum", na.rm = na.rm)
}

method(nd_min, Variable) <- function(x, dims, na.rm = FALSE) {
  reduce_variable(x, dims, "min", na.rm = na.rm)
}

method(nd_max, Variable) <- function(x, dims, na.rm = FALSE) {
  reduce_variable(x, dims, "max", na.rm = na.rm)
}


# --- DataArray methods ---

reduce_dataarray <- function(x, dims, fn, ...) {
  new_var <- fn(x@variable, dims, ...)

  # drop coords for reduced dims
  new_coords <- drop_indexes(x@coords, dims)

  DataArray(variable = new_var, coords = new_coords, name = x@name)
}

method(nd_mean, DataArray) <- function(x, dims, na.rm = FALSE) {
  reduce_dataarray(x, dims, nd_mean, na.rm = na.rm)
}

method(nd_sum, DataArray) <- function(x, dims, na.rm = FALSE) {
  reduce_dataarray(x, dims, nd_sum, na.rm = na.rm)
}

method(nd_min, DataArray) <- function(x, dims, na.rm = FALSE) {
  reduce_dataarray(x, dims, nd_min, na.rm = na.rm)
}

method(nd_max, DataArray) <- function(x, dims, na.rm = FALSE) {
  reduce_dataarray(x, dims, nd_max, na.rm = na.rm)
}


# --- Streaming reductions for lazy data ---

#' Reduce lazy (altarr) data over all but `keep_axes`, block by block
#'
#' The array is visited in blocks aligned to its chunk grid, grown from one
#' chunk towards `getOption("ndr.block_values")` values (reduced dimensions
#' first). Each block is one planned read; per-cell partial results (sum,
#' count, min, max) are combined across blocks, so memory holds one block
#' plus the result. Results follow base R's `sum()`, `mean()`, `min()` and
#' `max()` up to floating-point summation order.
#'
#' @param arr altarr array.
#' @param keep_axes integer, the dimensions to keep (ascending).
#' @param fn one of "mean", "sum", "min", "max".
#' @return An array of `dim(arr)[keep_axes]`, or a length-1 value.
#' @keywords internal
#' @noRd
reduce_lazy <- function(arr, keep_axes, fn, na.rm = FALSE) {
  d <- dim(arr)
  nd <- length(d)
  red_axes <- setdiff(seq_len(nd), keep_axes)
  block <- reduce_block_shape(d, lazy_chunk(arr), red_axes,
                              getOption("ndr.block_values", 2^22))

  kd <- d[keep_axes]
  ncell <- if (length(kd)) prod(kd) else 1
  cell <- if (length(kd)) array(seq_len(ncell), kd) else NULL
  acc <- if (fn %in% c("sum", "mean")) numeric(ncell)
         else rep(if (fn == "min") Inf else -Inf, ncell)
  cnt <- numeric(ncell)

  starts <- lapply(seq_len(nd), function(k) seq.int(1L, d[k], by = block[k]))
  grid <- expand.grid(lapply(starts, seq_along), KEEP.OUT.ATTRS = FALSE)
  for (b in seq_len(nrow(grid))) {
    subs <- lapply(seq_len(nd), function(k) {
      s0 <- starts[[k]][grid[[k]][b]]
      seq.int(s0, min(s0 + block[k] - 1L, d[k]))
    })
    v <- lazy_extract(arr, subs)
    if (length(keep_axes)) {
      v <- aperm(v, c(keep_axes, red_axes))
      lin <- as.vector(do.call(`[`, c(list(cell), subs[keep_axes])))
    } else {
      lin <- 1L
    }
    m <- matrix(v, nrow = length(lin))
    cnt[lin] <- cnt[lin] + if (na.rm) rowSums(!is.na(m)) else ncol(m)
    if (fn %in% c("sum", "mean")) {
      acc[lin] <- acc[lin] + rowSums(m, na.rm = na.rm)
    } else {
      acc[lin] <- row_extreme(m, acc[lin], fn, na.rm)
    }
  }

  out <- if (fn == "mean") acc / cnt else acc
  if (fn %in% c("min", "max") && any(cnt == 0)) {
    warning(sprintf("no non-missing arguments to %s; returning %s",
                    fn, if (fn == "min") "Inf" else "-Inf"), call. = FALSE)
  }
  if (is.integer(arr) && fn != "mean") out <- as_integer_result(out, fn)
  if (length(kd)) array(out, kd) else out
}

#' Running row-wise min or max of a block, combined with the previous values
#' @keywords internal
#' @noRd
row_extreme <- function(m, prev, fn, na.rm) {
  pfn <- if (fn == "min") pmin else pmax
  if (ncol(m) <= nrow(m)) {
    out <- prev
    for (j in seq_len(ncol(m))) out <- pfn(out, m[, j], na.rm = na.rm)
    out
  } else {
    f <- match.fun(fn)
    vals <- apply(m, 1L, function(r) {
      if (na.rm && all(is.na(r))) return(if (fn == "min") Inf else -Inf)
      f(r, na.rm = na.rm)
    })
    pfn(prev, vals, na.rm = na.rm)
  }
}

#' Integer input keeps integer sum/min/max results, as base R does
#' @keywords internal
#' @noRd
as_integer_result <- function(out, fn) {
  if (fn == "sum") {
    big <- !is.na(out) & abs(out) > .Machine$integer.max
    if (any(big)) {
      warning("integer overflow - use sum(as.numeric(.))", call. = FALSE)
      out[big] <- NA
    }
    return(as.integer(out))
  }
  # min/max of nothing is +/-Inf, which stays double (as in base R)
  if (any(is.infinite(out))) return(out)
  as.integer(out)
}

#' Block shape for streaming: a whole number of chunks along each dim,
#' grown towards `target` values, reduced dims first
#' @keywords internal
#' @noRd
reduce_block_shape <- function(d, chunk, red_axes, target) {
  block <- pmin(chunk, d)
  for (k in c(red_axes, setdiff(seq_along(d), red_axes))) {
    while (block[k] < d[k] &&
           prod(as.numeric(block)) / block[k] *
             min(d[k], block[k] + chunk[k]) <= target) {
      block[k] <- min(d[k], block[k] + chunk[k])
    }
  }
  block
}
