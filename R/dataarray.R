#' DataArray: a Variable with coordinates
#'
#' A DataArray wraps a Variable (the data) and attaches coordinates, so you
#' can do things like `sel(da, lat = 0, time = "2020-01-15")`. This is the
#' primary user-facing object for single-variable data.
#'
#' @param variable A Variable
#' @param coords Named list of [indexes] (ImplicitCoord, ExplicitCoord,
#'   AffineIndex, ...). Each index's dims must be among the Variable's `dims`,
#'   with matching sizes.
#' @param name Optional name for this data array (character, length 0 or 1).
#'
#' @examples
#' # 2D array with implicit spatial coords
#' v <- Variable(
#'   dims = c("lat", "lon"),
#'   data = matrix(rnorm(180 * 360), 180, 360),
#'   attrs = list(units = "K")
#' )
#' da <- DataArray(
#'   variable = v,
#'   coords = list(
#'     lat = ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0),
#'     lon = ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)
#'   ),
#'   name = "temperature"
#' )
#' da
#'
#' @export
DataArray <- new_class("DataArray",
  properties = list(
    variable = Variable,
    coords   = new_property(class_list, default = list()),
    name     = new_property(class_character, default = character())
  ),
  validator = function(self) {
    v <- self@variable
    s <- shape(v)
    vdims <- names(s)

    msg <- check_indexes(self@coords, s, "variable's dims")
    if (!is.null(msg)) return(msg)
    NULL
  }
)

# Convenience accessors

#' @export
method(ndim, DataArray) <- function(x) ndim(x@variable)

#' @export
method(shape, DataArray) <- function(x) shape(x@variable)

#' @export
`dim.ndr::DataArray` <- function(x) dim(x@variable)

#' @export
`length.ndr::DataArray` <- function(x) length(x@variable)

#' @export
`as.array.ndr::DataArray` <- function(x, ...) var_values(x@variable)
