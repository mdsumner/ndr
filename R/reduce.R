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
nd_mean <- new_generic("nd_mean", "x")

#' @rdname reductions
#' @export
nd_sum <- new_generic("nd_sum", "x")

#' @rdname reductions
#' @export
nd_min <- new_generic("nd_min", "x")

#' @rdname reductions
#' @export
nd_max <- new_generic("nd_max", "x")


# --- Variable methods ---

reduce_variable <- function(x, dims, fn, ...) {
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

  if (length(keep_axes) == 0L) {
    # reducing all dims → scalar
    val <- fn(arr, ...)
    return(Variable(dims = character(), data = array(val), attrs = x@attrs))
  }

  # apply over kept margins
  result <- apply(arr, keep_axes, fn, ...)

  # apply can return a vector when MARGIN is length 1 — ensure dim is set
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
  reduce_variable(x, dims, mean, na.rm = na.rm)
}

method(nd_sum, Variable) <- function(x, dims, na.rm = FALSE) {
  reduce_variable(x, dims, sum, na.rm = na.rm)
}

method(nd_min, Variable) <- function(x, dims, na.rm = FALSE) {
  reduce_variable(x, dims, min, na.rm = na.rm)
}

method(nd_max, Variable) <- function(x, dims, na.rm = FALSE) {
  reduce_variable(x, dims, max, na.rm = na.rm)
}


# --- DataArray methods ---

reduce_dataarray <- function(x, dims, fn, ...) {
  new_var <- fn(x@variable, dims, ...)

  # drop coords for reduced dims
  new_coords <- Filter(
    function(c) !coord_dim(c) %in% dims,
    x@coords
  )

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
