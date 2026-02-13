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


# --- helpers ---

#' Wrap a bare value as a scalar Variable
#' @noRd
wrap_scalar <- function(x) {
  Variable(dims = character(), data = array(x))
}

#' Apply a Variable-level broadcast op, wrapping scalars as needed
#' @noRd
var_op <- function(e1, e2, op) {
  if (!S7_inherits(e1, Variable)) e1 <- wrap_scalar(e1)
  if (!S7_inherits(e2, Variable)) e2 <- wrap_scalar(e2)
  broadcast_op(e1, e2, op)
}

#' Apply a DataArray-level op, wrapping scalars as needed
#' @noRd
da_op <- function(e1, e2, op) {
  # extract or wrap to Variable level
  v1 <- if (S7_inherits(e1, DataArray)) e1@variable
        else if (S7_inherits(e1, Variable)) e1
        else wrap_scalar(e1)
  v2 <- if (S7_inherits(e2, DataArray)) e2@variable
        else if (S7_inherits(e2, Variable)) e2
        else wrap_scalar(e2)

  result_var <- broadcast_op(v1, v2, op)

  # merge coords: take from left, fill in from right
  c1 <- if (S7_inherits(e1, DataArray)) e1@coords else list()
  c2 <- if (S7_inherits(e2, DataArray)) e2@coords else list()
  merged <- c2
  for (nm in names(c1)) merged[[nm]] <- c1[[nm]]
  merged <- Filter(function(c) coord_dim(c) %in% result_var@dims, merged)

  DataArray(variable = result_var, coords = merged, name = character())
}


# --- Register S7 methods for all arithmetic/comparison operators ---
#
# S7's Ops.S7_object intercepts before any S3 Ops.ClassName method,
# so we must register methods via S7's method() for each operator.

local({
  binary_ops <- list(`+`, `-`, `*`, `/`, `^`, `%%`, `%/%`,
                     `==`, `!=`, `<`, `<=`, `>`, `>=`)

  for (op in binary_ops) {
    local({
      my_op <- op

      # Variable <op> Variable
      method(my_op, list(Variable, Variable)) <- function(e1, e2) {
        var_op(e1, e2, my_op)
      }
      # Variable <op> scalar
      method(my_op, list(Variable, class_any)) <- function(e1, e2) {
        var_op(e1, e2, my_op)
      }
      # scalar <op> Variable
      method(my_op, list(class_any, Variable)) <- function(e1, e2) {
        var_op(e1, e2, my_op)
      }

      # DataArray <op> DataArray
      method(my_op, list(DataArray, DataArray)) <- function(e1, e2) {
        da_op(e1, e2, my_op)
      }
      # DataArray <op> anything (scalar or Variable)
      method(my_op, list(DataArray, class_any)) <- function(e1, e2) {
        da_op(e1, e2, my_op)
      }
      # anything <op> DataArray
      method(my_op, list(class_any, DataArray)) <- function(e1, e2) {
        da_op(e1, e2, my_op)
      }
    })
  }
})
