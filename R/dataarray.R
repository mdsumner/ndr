#' DataArray: a Variable with coordinates
#'
#' A DataArray wraps a Variable (the data) and attaches coordinates, so you
#' can do things like `sel(da, lat = 0, time = "2020-01-15")`. This is the
#' primary user-facing object for single-variable data.
#'
#' @param variable A Variable
#' @param coords Named list of coordinate objects (ImplicitCoord or ExplicitCoord).
#'   Each coordinate's `dim` property must match one of the Variable's `dims`.
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
#' @importFrom S7 class_character
#' @export
DataArray <- new_class("DataArray",
  properties = list(
    variable = Variable,
    coords   = new_property(class_list, default = list()),
    name     = new_property(S7::class_character, default = character())
  ),
  validator = function(self) {
    v <- self@variable
    s <- shape(v)
    vdims <- names(s)

    for (nm in names(self@coords)) {
      coord <- self@coords[[nm]]
      cdim <- coord_dim(coord)
      if (!cdim %in% vdims) {
        return(sprintf(
          "coordinate '%s' references dim '%s' which is not in the variable's dims (%s)",
          nm, cdim, paste(vdims, collapse = ", ")
        ))
      }
      clen <- coord_length(coord)
      if (clen != s[cdim]) {
        return(sprintf(
          "coordinate '%s' has length %d but dim '%s' has size %d",
          nm, clen, cdim, s[cdim]
        ))
      }
    }
    NULL
  }
)

# Convenience accessors

#' @export
method(ndim, DataArray) <- function(x) ndim(x@variable)

#' @export
method(shape, DataArray) <- function(x) shape(x@variable)

#' @export
dim.DataArray <- function(x) dim(x@variable)

#' @export
length.DataArray <- function(x) length(x@variable)

#' @export
as.array.DataArray <- function(x, ...) as.array(x@variable)
