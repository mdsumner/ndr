#' Arithmetic operations on Variables and DataArrays
#'
#' All arithmetic (`+`, `-`, `*`, `/`, etc.) and comparison (`==`, `<`, etc.)
#' operators are supported. Operations broadcast by dimension name: dimensions
#' present in one operand but not the other are automatically expanded.
#'
#' @name ops
#' @examples
#' # Broadcasting: 3D temperature * 2D land mask
#' temp <- Variable(
#'   dims = c("time", "lat", "lon"),
#'   data = array(rnorm(10 * 3 * 4), dim = c(10, 3, 4))
#' )
#' mask <- Variable(
#'   dims = c("lat", "lon"),
#'   data = matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0), 3, 4)
#' )
#'
#' result <- temp * mask
#' shape(result)  # time=10, lat=3, lon=4
#'
#' # Scalar operations
#' temp + 273.15
#' temp * 2
#'
NULL

#' @export
Ops.Variable <- function(e1, e2) {
  op <- match.fun(.Generic)

  if (missing(e2)) {
    # unary ops: -, +, !
    return(Variable(
      dims = e1@dims,
      data = op(e1@data),
      attrs = e1@attrs
    ))
  }

  # wrap bare values
  if (!S7_inherits(e1, Variable) && !S7_inherits(e1, DataArray)) {
    e1 <- Variable(dims = character(), data = array(e1))
  }
  if (!S7_inherits(e2, Variable) && !S7_inherits(e2, DataArray)) {
    e2 <- Variable(dims = character(), data = array(e2))
  }

  # if either is a DataArray, delegate to DataArray ops
  if (S7_inherits(e1, DataArray) || S7_inherits(e2, DataArray)) {
    return(dataarray_op(e1, e2, op))
  }

  broadcast_op(e1, e2, op)
}


#' @export
Ops.DataArray <- function(e1, e2) {
  op <- match.fun(.Generic)

  if (missing(e2)) {
    return(DataArray(
      variable = Variable(
        dims = e1@variable@dims,
        data = op(e1@variable@data),
        attrs = e1@variable@attrs
      ),
      coords = e1@coords,
      name = e1@name
    ))
  }

  # wrap bare values
  if (!S7_inherits(e1, Variable) && !S7_inherits(e1, DataArray)) {
    e1 <- Variable(dims = character(), data = array(e1))
  }
  if (!S7_inherits(e2, Variable) && !S7_inherits(e2, DataArray)) {
    e2 <- Variable(dims = character(), data = array(e2))
  }

  dataarray_op(e1, e2, op)
}


#' Apply an operation between Variables/DataArrays, preserving coordinates
#' @keywords internal
dataarray_op <- function(e1, e2, op) {
  # extract Variables
  v1 <- if (S7_inherits(e1, DataArray)) e1@variable else e1
  v2 <- if (S7_inherits(e2, DataArray)) e2@variable else e2

  # broadcast the underlying Variables
  result_var <- broadcast_op(v1, v2, op)

  # merge coordinates: take from left, fill in from right
  c1 <- if (S7_inherits(e1, DataArray)) e1@coords else list()
  c2 <- if (S7_inherits(e2, DataArray)) e2@coords else list()

  merged_coords <- c2  # start with right
  for (nm in names(c1)) {
    merged_coords[[nm]] <- c1[[nm]]  # left overwrites
  }
  # only keep coords whose dim is in the result
  result_dims <- result_var@dims
  merged_coords <- Filter(
    function(c) coord_dim(c) %in% result_dims,
    merged_coords
  )

  DataArray(
    variable = result_var,
    coords = merged_coords,
    name = character()
  )
}
