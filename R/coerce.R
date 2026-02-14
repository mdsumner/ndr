#' Coerce to data frame (long format)
#'
#' Expands a DataArray into a data frame with one row per cell, including
#' coordinate values for each dimension. Compatible with ggplot2 and dplyr.
#'
#' @param x A DataArray
#' @param row.names Ignored
#' @param optional Ignored
#' @param ... Ignored
#' @return A data.frame
#'
#' @examples
#' da <- DataArray(
#'   variable = Variable(
#'     dims = c("lat", "lon"),
#'     data = matrix(1:6, 2, 3)
#'   ),
#'   coords = list(
#'     lat = ExplicitCoord(dimension = "lat", values = c(10, 20)),
#'     lon = ExplicitCoord(dimension = "lon", values = c(100, 110, 120))
#'   ),
#'   name = "value"
#' )
#' as.data.frame(da)
#'
#' @export
`as.data.frame.ndr::DataArray` <- function(x, row.names = NULL, optional = FALSE, ...) {
  s <- shape(x@variable)
  dims <- names(s)

  # build coordinate vectors (or integer indices if no coord)
  coord_vals <- list()
  for (d in dims) {
    # find coord for this dim
    coord <- NULL
    for (c in x@coords) {
      if (coord_dim(c) == d) { coord <- c; break }
    }
    if (!is.null(coord)) {
      coord_vals[[d]] <- coord_values(coord)
    } else {
      coord_vals[[d]] <- seq_len(s[d])
    }
  }

  # expand grid (respects R's column-major array layout)
  grid <- expand.grid(coord_vals, KEEP.OUT.ATTRS = FALSE)

  # add the data values
  val_name <- if (length(x@name) > 0L) x@name else "value"
  grid[[val_name]] <- as.vector(x@variable@data)

  grid
}


#' @export
`as.data.frame.ndr::LazyDataArray` <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(collect(x), row.names = row.names, optional = optional, ...)
}


#' Convert a Dataset to a list of data frames
#' @param x A Dataset
#' @param ... Ignored
#' @return A named list of data.frames, one per data variable
#' @export
`as.list.ndr::Dataset` <- function(x, ...) {
  lapply(names(x@data_vars), function(nm) {
    da <- x[[nm]]
    as.data.frame(da)
  })
}


#' Quick Variable constructor from a named-dim array
#'
#' @param data An R array
#' @param ... Dimension names as `name = size` pairs (ignored, names used)
#'   OR a character vector of dim names
#' @return A Variable
#'
#' @examples
#' # From a matrix with named dims
#' v <- as_variable(matrix(1:12, 3, 4), lat = 3, lon = 4)
#'
#' @export
as_variable <- function(data, ...) {
  args <- list(...)
  if (length(args) == 1L && is.character(args[[1]])) {
    dims <- args[[1]]
  } else {
    dims <- names(args)
  }

  if (is.null(dim(data))) {
    if (length(dims) == 1L) {
      dim(data) <- length(data)
    } else {
      stop("bare vector data requires exactly one dim name")
    }
  }

  if (length(dims) != length(dim(data))) {
    stop("number of dim names must match number of array dimensions")
  }

  Variable(dims = dims, data = data)
}
