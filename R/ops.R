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


# --- Unary operators ---
#
# S7 uses class_missing for the absent second argument in unary ops.

method(`-`, list(Variable, class_missing)) <- function(e1, e2) {
  Variable(dims = e1@dims, data = -var_data(e1))
}
method(`+`, list(Variable, class_missing)) <- function(e1, e2) e1

method(`-`, list(DataArray, class_missing)) <- function(e1, e2) {
  v <- e1@variable
  DataArray(
    variable = Variable(dims = v@dims, data = -var_data(v)),
    coords = e1@coords, name = e1@name
  )
}
method(`+`, list(DataArray, class_missing)) <- function(e1, e2) e1


# --- Binary operators: Variable ---
#
# Each operator needs three signatures:
#   Variable <op> Variable
#   Variable <op> scalar
#   scalar   <op> Variable
#
# Explicit top-level registration — S7's method<- needs this to
# discover and re-register methods at package load time. A local()
# loop fails because the calling frame is anonymous.

# +
method(`+`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `+`)
method(`+`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `+`)
method(`+`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `+`)

# -
method(`-`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `-`)
method(`-`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `-`)
method(`-`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `-`)

# *
method(`*`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `*`)
method(`*`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `*`)
method(`*`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `*`)

# /
method(`/`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `/`)
method(`/`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `/`)
method(`/`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `/`)

# ^
method(`^`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `^`)
method(`^`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `^`)
method(`^`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `^`)

# %%
method(`%%`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `%%`)
method(`%%`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `%%`)
method(`%%`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `%%`)

# %/%
method(`%/%`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `%/%`)
method(`%/%`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `%/%`)
method(`%/%`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `%/%`)

# ==
method(`==`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `==`)
method(`==`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `==`)
method(`==`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `==`)

# !=
method(`!=`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `!=`)
method(`!=`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `!=`)
method(`!=`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `!=`)

# <
method(`<`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `<`)
method(`<`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `<`)
method(`<`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `<`)

# <=
method(`<=`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `<=`)
method(`<=`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `<=`)
method(`<=`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `<=`)

# >
method(`>`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `>`)
method(`>`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `>`)
method(`>`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `>`)

# >=
method(`>=`, list(Variable, Variable))  <- function(e1, e2) var_op(e1, e2, `>=`)
method(`>=`, list(Variable, class_any)) <- function(e1, e2) var_op(e1, e2, `>=`)
method(`>=`, list(class_any, Variable)) <- function(e1, e2) var_op(e1, e2, `>=`)


# --- Binary operators: DataArray ---

# +
method(`+`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `+`)
method(`+`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `+`)
method(`+`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `+`)

# -
method(`-`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `-`)
method(`-`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `-`)
method(`-`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `-`)

# *
method(`*`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `*`)
method(`*`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `*`)
method(`*`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `*`)

# /
method(`/`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `/`)
method(`/`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `/`)
method(`/`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `/`)

# ^
method(`^`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `^`)
method(`^`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `^`)
method(`^`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `^`)

# %%
method(`%%`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `%%`)
method(`%%`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `%%`)
method(`%%`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `%%`)

# %/%
method(`%/%`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `%/%`)
method(`%/%`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `%/%`)
method(`%/%`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `%/%`)

# ==
method(`==`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `==`)
method(`==`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `==`)
method(`==`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `==`)

# !=
method(`!=`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `!=`)
method(`!=`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `!=`)
method(`!=`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `!=`)

# <
method(`<`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `<`)
method(`<`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `<`)
method(`<`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `<`)

# <=
method(`<=`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `<=`)
method(`<=`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `<=`)
method(`<=`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `<=`)

# >
method(`>`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `>`)
method(`>`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `>`)
method(`>`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `>`)

# >=
method(`>=`, list(DataArray, DataArray)) <- function(e1, e2) da_op(e1, e2, `>=`)
method(`>=`, list(DataArray, class_any)) <- function(e1, e2) da_op(e1, e2, `>=`)
method(`>=`, list(class_any, DataArray)) <- function(e1, e2) da_op(e1, e2, `>=`)
