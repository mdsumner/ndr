#' Variable: a named-dimension array
#'
#' The fundamental building block. A Variable is an N-dimensional array that
#' knows the names of its dimensions. It does NOT know about coordinates —
#' that's DataArray's job.
#'
#' @param dims Character vector of dimension names.
#' @param data An R array (or matrix, or vector with dim attribute).
#' @param attrs Named list of arbitrary metadata.
#' @param encoding Named list of on-disk encoding info (scale_factor, etc.).
#'
#' @examples
#' # A 3D temperature field
#' temp_data <- array(rnorm(365 * 180 * 360), dim = c(365, 180, 360))
#' v <- Variable(
#'   dims = c("time", "lat", "lon"),
#'   data = temp_data,
#'   attrs = list(units = "K", long_name = "Temperature")
#' )
#' v
#'
#' # Scalar variable
#' s <- Variable(dims = character(), data = array(42))
#' s
#'
#' @export
Variable <- S7::new_class("Variable",
  properties = list(
    dims  = S7::class_character,
    data  = class_any,
    attrs = new_property(class_list, default = list()),
    encoding = new_property(class_list, default = list())
  ),
  validator = function(self) {
    d <- self@data
    nd <- length(dim(d))
    # allow scalar: 0 dims, data is length-1 array
    if (length(self@dims) == 0L) {
      if (length(d) != 1L) return("scalar Variable must have length-1 data")
      return(NULL)
    }
    if (nd == 0L) {
      # bare vector — treat as 1D
      if (length(self@dims) != 1L)
        return("vector data requires exactly one dim name")
      return(NULL)
    }
    if (length(self@dims) != nd) {
      return(sprintf(
        "length(dims) is %d but data has %d dimensions",
        length(self@dims), nd
      ))
    }
    if (anyDuplicated(self@dims))
      return("dims must be unique")
    NULL
  }
)

#' Get the shape of a Variable
#' @param x A Variable
#' @return Named integer vector of dimension lengths
#' @export
ndim <- new_generic("ndim", "x")

method(ndim, Variable) <- function(x) {
  length(x@dims)
}

#' @export
shape <- new_generic("shape", "x")

method(shape, Variable) <- function(x) {
  if (length(x@dims) == 0L) return(stats::setNames(integer(), character()))
  d <- dim(x@data)
  if (is.null(d)) d <- length(x@data)
  stats::setNames(as.integer(d), x@dims)
}

# dim method for Variable (S3, so base::dim dispatches)
#' @export
dim.ndr_Variable <- function(x) {
  s <- shape(x)
  if (length(s) == 0L) return(NULL)
  unname(s)
}

#' @export
length.ndr_Variable <- function(x) {
  prod(shape(x))
}

# Ensure Variable is an array when needed
#' @export
as.array.ndr_Variable <- function(x, ...) {
  d <- x@data
  if (is.null(dim(d)) && length(x@dims) == 1L) {
    dim(d) <- length(d)
  }
  d
}
