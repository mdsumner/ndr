#' Coordinate-based indexing
#'
#' `sel()` selects data by coordinate value (label-based).
#' `isel()` selects data by integer index (position-based).
#'
#' Both work on Variables, DataArrays, and Datasets, and return objects of
#' the same type with dimensions correctly updated.
#'
#' @param .data A Variable, DataArray, or Dataset
#' @param ... Named arguments specifying the selection. For `sel()`, values
#'   are coordinate values. For `isel()`, values are integer indices (1-based).
#'   Use a vector for ranges: `sel(da, lat = c(-30, 30))` selects the range.
#'   Use a scalar for a single position: `isel(da, time = 1)` picks one index
#'   and drops that dimension.
#'
#' @return Same type as input, with dimensions sliced or dropped.
#'
#' @examples
#' v <- Variable(
#'   dims = c("lat", "lon"),
#'   data = matrix(1:12, 3, 4)
#' )
#' da <- DataArray(
#'   variable = v,
#'   coords = list(
#'     lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10),
#'     lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100, step = 10)
#'   )
#' )
#'
#' # Select by coordinate value
#' sel(da, lat = 0)             # single latitude, drops lat dim
#' sel(da, lat = c(-10, 0))     # range of latitudes
#'
#' # Select by integer index
#' isel(da, lon = 1:2)          # first two longitude columns
#' isel(da, lat = 2, lon = 3)   # single cell
#'
#' @name indexing
NULL


#' @rdname indexing
#' @export
isel <- new_generic("isel", ".data")

method(isel, Variable) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  s <- shape(.data)
  dims <- names(s)
  arr <- var_data(.data)

  # build index list: NULL means "take all" for that dim
  idx <- rep(list(TRUE), length(dims))
  names(idx) <- dims
  drop_dims <- character()

  for (d in names(selections)) {
    if (!d %in% dims) stop(sprintf("dimension '%s' not found", d))
    val <- selections[[d]]
    idx[[d]] <- val
    # scalar selection drops the dimension
    if (length(val) == 1L) {
      drop_dims <- c(drop_dims, d)
    }
  }

  # lazy data: a lazy view of the selection, nothing is read
  if (is_lazy(arr)) {
    full <- mapply(norm_index, idx, s, SIMPLIFY = FALSE)
    keep <- !(dims %in% drop_dims & lengths(full) == 1L)
    if (!any(keep)) {
      value <- lazy_extract(arr, full)
      return(Variable(dims = character(), data = array(as.vector(value)),
                      attrs = .data@attrs))
    }
    if (any(lengths(full) == 0L)) {
      # an empty selection has nothing to read
      result <- array(vector(typeof(arr)), dim = lengths(full)[keep])
    } else {
      result <- lazy_view(arr, full, keep)
    }
    return(Variable(dims = dims[keep], data = result, attrs = .data@attrs))
  }

  # subset the array
  result <- do.call(`[`, c(list(arr), unname(idx), list(drop = FALSE)))

  # determine new dims and shape
  new_dims <- setdiff(dims, drop_dims)
  if (length(new_dims) == 0L) {
    # scalar result
    return(Variable(dims = character(), data = array(as.vector(result)),
                    attrs = .data@attrs))
  }

  # drop the singleton dimensions
  new_shape <- dim(result)[!dims %in% drop_dims]
  dim(result) <- new_shape

  Variable(dims = new_dims, data = result, attrs = .data@attrs)
}


method(isel, DataArray) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  # apply isel to the underlying Variable
  new_var <- isel(.data@variable, ...)

  # each index works out its own subset
  new_coords <- isel_indexes(.data@coords, selections)

  DataArray(variable = new_var, coords = new_coords, name = .data@name)
}


method(isel, Dataset) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  new_vars <- lapply(.data@data_vars, function(v) {
    # only apply selections for dims this variable has
    v_sels <- selections[names(selections) %in% v@dims]
    if (length(v_sels) == 0L) return(v)
    do.call(isel, c(list(v), v_sels))
  })

  new_coords <- isel_indexes(.data@coords, selections)

  Dataset(data_vars = new_vars, coords = new_coords, attrs = .data@attrs)
}


# --- sel: label-based indexing ---

#' @rdname indexing
#' @export
sel <- new_generic("sel", ".data")

method(sel, DataArray) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  # each index turns its labels into integer positions
  int_sels <- sel_positions(.data@coords, selections)
  do.call(isel, c(list(.data), int_sels))
}


method(sel, Dataset) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  int_sels <- sel_positions(.data@coords, selections)
  do.call(isel, c(list(.data), int_sels))
}
