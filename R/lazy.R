#' Lazy data in a Variable
#'
#' A Variable's `data` can be a lazy chunked array from the altarr package:
#' an ordinary double, integer or logical vector with a `dim` attribute that
#' reads its values chunk by chunk only when they are asked for. Variables
#' read by [open_dataset()] hold such arrays, and any altarr array can be
#' used directly:
#'
#' ```
#' v <- Variable(dims = c("lon", "lat", "time"), data = altarr::altarr(...))
#' ```
#'
#' Every Variable, DataArray and Dataset method accepts lazy data:
#'
#' * `dim()`, `shape()`, printing and coordinate work read nothing.
#' * `isel()` and `sel()` read nothing: they return a Variable whose data is
#'   a lazy view of the selection, still backed by the same chunks.
#' * Reductions ([nd_mean()] and friends) stream the array in blocks aligned
#'   to its chunks, one planned read per block, and never hold more than one
#'   block plus the result in memory.
#' * Arithmetic, `as.array()`, `as.data.frame()` and `collect()` read the
#'   (selected) values into memory with one planned read.
#'
#' The block size for reductions is `getOption("ndr.block_values")` values
#' (default `2^22`); the chunk cache and fetch batching are altarr's options
#' (see `?altarr::altarr_contract`).
#'
#' @name lazy-data
NULL


# --- collect() ---

#' Read lazy data into memory
#'
#' Replaces lazy (altarr) data with an ordinary in-memory array, reading it
#' with one planned read. Objects whose data is already in memory are
#' returned unchanged.
#'
#' @param x A Variable, DataArray or Dataset.
#' @param ... Unused.
#' @return An object of the same class with data in memory.
#' @seealso [lazy-data]
#' @export
collect <- S7::new_generic("collect", "x")

S7::method(collect, Variable) <- function(x) {
  if (!is_lazy(x@data)) return(x)
  Variable(dims = x@dims, data = var_values(x), attrs = x@attrs,
           encoding = x@encoding)
}

S7::method(collect, DataArray) <- function(x) {
  if (!is_lazy(x@variable@data)) return(x)
  DataArray(variable = collect(x@variable), coords = x@coords, name = x@name)
}

S7::method(collect, Dataset) <- function(x) {
  Dataset(data_vars = lapply(x@data_vars, collect), coords = x@coords,
          attrs = x@attrs, .backend = x@.backend)
}


# --- helpers for lazy (altarr) data ---

#' Is this lazy altarr data?
#'
#' An altarr array can only exist once altarr's namespace is loaded (its
#' ALTREP classes are registered then), so the check never loads altarr.
#' @keywords internal
#' @noRd
is_lazy <- function(x) {
  isNamespaceLoaded("altarr") && altarr::is_altarr(x)
}

#' Rectangular read of lazy data: one planned fetch
#' @param x altarr array
#' @param subs list of 1-based integer subscripts, one per dimension
#' @keywords internal
#' @noRd
lazy_extract <- function(x, subs) {
  do.call(altarr::altarr_extract, c(list(x), unname(subs), list(drop = FALSE)))
}

#' Chunk shape of lazy data (clipped to the array's extent)
#' @keywords internal
#' @noRd
lazy_chunk <- function(x) {
  nd <- length(dim(x))
  p <- do.call(altarr::altarr_plan, c(list(x), as.list(rep(1L, nd))))
  as.integer(unlist(p[1L, paste0(names(p)[seq_len(nd)], "_n")]))
}

#' A Variable's values as an in-memory array
#'
#' Lazy data is read with one planned read, so this is the place where
#' arithmetic, coercion and collect() pay for their values.
#' @keywords internal
#' @noRd
var_values <- function(x) {
  d <- var_data(x)
  if (!is_lazy(d)) return(d)
  lazy_extract(d, lapply(dim(d), seq_len))
}

#' Normalise one subscript to 1-based integers, by base R's own rules
#' @keywords internal
#' @noRd
norm_index <- function(i, n) {
  if (isTRUE(i)) return(seq_len(n))
  if (is.numeric(i) && any(i > n, na.rm = TRUE)) stop("subscript out of bounds")
  out <- seq_len(n)[i]
  if (anyNA(out)) stop("NA subscripts are not supported for lazy data")
  out
}

#' A lazy view of a selection from lazy data
#'
#' Returns a new altarr array of the selected positions. Its chunk shape is
#' the source's, and its fetch reads each batch of requested chunks with one
#' planned read of the source, so the view keeps the source's chunk-aware
#' behaviour: selecting reads nothing, and later reads touch only the source
#' chunks that hold selected values.
#'
#' @param src altarr array.
#' @param idx list of 1-based integer subscripts, one per source dimension.
#' @param keep logical, which source dimensions the view keeps (dropped
#'   dimensions must have a single index).
#' @param transform optional function applied to each chunk's values (used
#'   for CF unpacking); the view is then double.
#' @keywords internal
#' @noRd
lazy_view <- function(src, idx, keep, transform = NULL) {
  vdim <- lengths(idx)[keep]
  chunk <- pmin(lazy_chunk(src)[keep], vdim)
  type <- if (is.null(transform)) typeof(src) else "double"
  altarr::altarr(vdim, chunk, view_fetch(src, idx, keep, vdim, chunk, transform),
                 type = type)
}

#' Fetch function for lazy_view(), built in a factory so the recipe carries
#' only the source array and the selection
#' @keywords internal
#' @noRd
view_fetch <- function(src, idx, keep, vdim, chunk, transform = NULL) {
  force(src); force(idx); force(keep); force(vdim); force(chunk); force(transform)
  function(chunks) {
    out <- view_read(chunks, src, idx, keep, vdim, chunk)
    if (is.null(transform)) out else lapply(out, transform)
  }
}

#' @keywords internal
#' @noRd
view_read <- function(chunks, src, idx, keep, vdim, chunk) {
  n <- nrow(chunks)
  ranges <- lapply(seq_len(n), function(r) {
    start <- chunks[r, ] * chunk + 1L
    end <- pmin(start + chunk - 1L, vdim)
    Map(seq.int, start, end)
  })
  ## one read of the union of the batch's positions, unless the batch is
  ## so scattered that the union's cartesian product would be much larger
  ## than the chunks themselves (then one read per chunk)
  u <- lapply(seq_along(vdim), function(k) {
    sort(unique(unlist(lapply(ranges, `[[`, k))))
  })
  need <- sum(vapply(ranges, function(rg) prod(as.numeric(lengths(rg))), 1))
  if (prod(as.numeric(lengths(u))) <= 4 * need) {
    block <- lazy_extract(src, view_subs(idx, keep, u))
    dim(block) <- lengths(u)
    lapply(ranges, function(rg) {
      pos <- Map(match, rg, u)
      as.vector(do.call(`[`, c(list(block), pos, list(drop = FALSE))))
    })
  } else {
    lapply(ranges, function(rg) as.vector(lazy_extract(src, view_subs(idx, keep, rg))))
  }
}

#' Map view positions (kept dims only) to source subscripts (all dims)
#' @keywords internal
#' @noRd
view_subs <- function(idx, keep, pos) {
  subs <- idx
  subs[keep] <- Map(`[`, idx[keep], pos)
  subs
}


# --- GDAL multidim arrays as lazy data ---

#' A lazy array for one GDAL multidim variable
#'
#' Made by GDAL7's `as_altarr()`: chunks are the array's own storage chunks
#' (`block_size`), each batch of chunks is one advised GDAL read, and nodata
#' reads as `NA`. A saved Variable carries only the dsn and array name.
#' GDAL7 does not apply CF `scale_factor`/`add_offset`, so a packed array is
#' unpacked here, in a lazy view that keeps the same chunks.
#'
#' @param dsn Data source name.
#' @param var_name Array name, or a full path from the root group.
#' @return An altarr array in R (column-major) dimension order.
#' @keywords internal
#' @noRd
gdal_lazy_array <- function(dsn, var_name) {
  ds <- GDAL7::gdal_open(dsn, multidim = TRUE)
  on.exit(GDAL7::gdal_close(ds), add = TRUE)
  arr <- GDAL7::open_mdarray(GDAL7::get_root_group(ds), var_name)
  scale <- arr@scale
  offset <- arr@offset

  x <- GDAL7::as_altarr(dsn, array = var_name)
  # Variable@dims carries the names; keep the data a plain array
  dimnames(x) <- NULL

  scale <- if (length(scale) == 1L && !is.na(scale)) scale else 1
  offset <- if (length(offset) == 1L && !is.na(offset)) offset else 0
  if (scale != 1 || offset != 0) {
    x <- lazy_view(x, lapply(dim(x), seq_len), rep(TRUE, length(dim(x))),
                   transform = unpack_fun(scale, offset))
  }
  x
}

#' CF unpacking as a chunk transform, built in a factory for small recipes
#' @keywords internal
#' @noRd
unpack_fun <- function(scale, offset) {
  force(scale); force(offset)
  function(v) v * scale + offset
}
