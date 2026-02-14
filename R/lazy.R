#' @title LazyDataArray
#'
#' @description
#' A lazy representation of a DataArray that carries a selection spec rather
#' than data. Created when accessing variables from a Dataset opened via
#' `open_dataset()`. Data is only read from disk when `collect()` is called.
#'
#' The selection spec accumulates as `sel()`/`isel()` calls are chained,
#' recording which dimensions have been narrowed and to what index ranges.
#' `collect()` translates the accumulated spec into a single GDAL
#' `mdim_array_read()` hyperslab call.
#'
#' @name LazyDataArray
NULL


# --- LazyDataArray class definition ---

#' @export
LazyDataArray <- S7::new_class("LazyDataArray",
  properties = list(
    #' @field name Character. Variable name.
    name = S7::new_property(S7::class_character, default = character()),

    #' @field dims Character vector. Dimension names in R (column-major) order.
    dims = S7::class_character,

    #' @field dim_sizes Integer vector. Full (unsliced) dimension sizes in R order.
    dim_sizes = S7::class_integer,

    #' @field coords Named list of Coordinate objects (same as DataArray).
    coords = S7::new_property(S7::class_list, default = list()),

    #' @field attrs Named list of attributes.
    attrs = S7::new_property(S7::class_list, default = list()),

    #' @field .selection Named list of selection specs per dimension.
    #'   NULL = full dimension (no selection applied).
    #'   Integer vector = selected 1-based R indices for that dimension.
    .selection = S7::new_property(S7::class_list, default = list()),

    #' @field .backend List with `dsn` and `var_name` for GDAL reads.
    .backend = S7::class_any
  ),
  validator = function(self) {
    if (length(self@dims) != length(self@dim_sizes)) {
      "dims and dim_sizes must have the same length"
    }
  }
)


# --- Schema-based accessors (no data read needed) ---

S7::method(ndim, LazyDataArray) <- function(x) {
  # Number of dims after accounting for scalar selections (which drop dims)
  sum(vapply(x@dims, function(dn) {
    sel <- x@.selection[[dn]]
    is.null(sel) || length(sel) > 1L
  }, logical(1L)))
}

S7::method(shape, LazyDataArray) <- function(x) {
  out <- integer()
  nms <- character()
  for (i in seq_along(x@dims)) {
    dn <- x@dims[i]
    sel <- x@.selection[[dn]]
    if (is.null(sel)) {
      out <- c(out, x@dim_sizes[i])
      nms <- c(nms, dn)
    } else if (length(sel) > 1L) {
      out <- c(out, length(sel))
      nms <- c(nms, dn)
    }
    # scalar selection: dimension dropped, not included
  }
  stats::setNames(out, nms)
}

#' @export
`dim.ndr::LazyDataArray` <- function(x) {
  s <- shape(x)
  if (length(s) == 0L) return(NULL)
  unname(s)
}

#' @export
`length.ndr::LazyDataArray` <- function(x) {
  prod(shape(x))
}

#' @export
`as.array.ndr::LazyDataArray` <- function(x, ...) {
  as.array(collect(x), ...)
}


# --- collect() generic and methods ---

#' Materialise a lazy object by reading data from disk
#'
#' @param x A LazyDataArray (or DataArray, which is returned as-is).
#' @return A DataArray with data in memory.
#' @export
collect <- S7::new_generic("collect", "x")


#' @export
S7::method(collect, LazyDataArray) <- function(x) {
  be <- x@.backend
  spec <- x@.selection

  # Open GDAL handle
  ds_handle <- new(
    gdalraster_class("GDALMultiDimRaster"),
    be$dsn, TRUE, character(), FALSE
  )
  on.exit(ds_handle$close(), add = TRUE)

  arr <- ds_handle$openArrayFromFullname(paste0("/", be$var_name), character())

  # Translate selection spec to start/count in GDAL C-order
  hyperslab <- selection_to_hyperslab(x@dims, x@dim_sizes, spec)

  # Read hyperslab
  data <- gdalraster_fn("mdim_array_read")(
    arr,
    start = hyperslab$start,
    count = hyperslab$count
  )

  # gis$dim and gis$dim_names are in R order
  gis <- attr(data, "gis")

  # Build sliced coordinate list
  sliced_coords <- slice_coords(x@coords, x@dims, spec)

  # Determine output dims (drop dimensions with size 1 from scalar selection)
  out_dims <- character()
  out_dim_sizes <- integer()
  for (i in seq_along(x@dims)) {
    dn <- x@dims[i]
    sel <- spec[[dn]]
    if (is.null(sel)) {
      out_dims <- c(out_dims, dn)
      out_dim_sizes <- c(out_dim_sizes, x@dim_sizes[i])
    } else if (length(sel) > 1L) {
      out_dims <- c(out_dims, dn)
      out_dim_sizes <- c(out_dim_sizes, length(sel))
    }
    # length(sel) == 1 → dimension is dropped (scalar selection)
  }

  # Reshape data into R array with correct dims
  arr_data <- array(as.vector(data), dim = if (length(out_dim_sizes) > 0L) out_dim_sizes else NULL)

  # Build Variable
  var <- Variable(
    dims  = out_dims,
    data  = arr_data,
    attrs = x@attrs
  )

  # Build DataArray
  DataArray(
    name   = x@name,
    variable = var,
    coords = sliced_coords
  )
}


#' @export
S7::method(collect, DataArray) <- function(x) x


# --- Hyperslab translation ---

#' Translate R-order selection spec to GDAL C-order start/count
#'
#' @param dims Character vector of dimension names in R (F) order.
#' @param dim_sizes Integer vector of full dimension sizes in R order.
#' @param selection Named list: dim_name → integer indices (1-based) or NULL.
#' @return List with `start` (0-based, C-order) and `count` (C-order).
#' @keywords internal
#' @noRd
selection_to_hyperslab <- function(dims, dim_sizes, selection) {
  n <- length(dims)
  r_start <- integer(n)
  r_count <- integer(n)

  for (i in seq_len(n)) {
    dn <- dims[i]
    sel <- selection[[dn]]

    if (is.null(sel)) {
      # Full dimension
      r_start[i] <- 0L   # 0-based for GDAL
      r_count[i] <- dim_sizes[i]
    } else {
      # Contiguous selection: use min/max of indices
      # (for non-contiguous, we read the bounding box and subset later)
      idx <- as.integer(sel)
      r_start[i] <- min(idx) - 1L   # convert 1-based → 0-based
      r_count[i] <- max(idx) - min(idx) + 1L
    }
  }

  # GDAL expects C-order (reversed from R F-order)
  list(
    start = rev(r_start),
    count = rev(r_count)
  )
}


#' Slice coordinates to match selection
#'
#' @param coords Named list of Coordinate objects.
#' @param dims Character vector of dimension names in R order.
#' @param selection Named list: dim_name → integer indices or NULL.
#' @return Named list of Coordinate objects, sliced and filtered.
#' @keywords internal
#' @noRd
slice_coords <- function(coords, dims, selection) {
  out <- list()
  for (nm in names(coords)) {
    coord <- coords[[nm]]
    dn <- coord_dim(coord)

    sel <- selection[[dn]]

    if (is.null(sel)) {
      # No selection on this dim: keep as-is
      out[[nm]] <- coord
    } else if (length(sel) == 1L) {
      # Scalar selection: drop this coord (dimension will be dropped)
    } else {
      # Subset coordinate using coord_slice
      out[[nm]] <- coord_slice(coord, sel)
    }
  }
  out
}
