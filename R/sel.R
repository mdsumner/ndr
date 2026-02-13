#' Coordinate-based indexing
#'
#' `sel()` selects data by coordinate value (label-based).
#' `isel()` selects data by integer index (position-based).
#'
#' Both work on Variables, DataArrays, and Datasets, and return objects of
#' the same type with dimensions correctly updated.
#'
#' @param x A Variable, DataArray, or Dataset
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
isel <- new_generic("isel", "x")

method(isel, Variable) <- function(x, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(x)

  s <- shape(x)
  dims <- names(s)
  arr <- as.array(x)

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

  # subset the array
  result <- do.call(`[`, c(list(arr), unname(idx), list(drop = FALSE)))

  # determine new dims and shape
  new_dims <- setdiff(dims, drop_dims)
  if (length(new_dims) == 0L) {
    # scalar result
    return(Variable(dims = character(), data = array(as.vector(result)),
                    attrs = x@attrs))
  }

  # drop the singleton dimensions
  new_shape <- dim(result)[!dims %in% drop_dims]
  dim(result) <- new_shape

  Variable(dims = new_dims, data = result, attrs = x@attrs)
}


method(isel, DataArray) <- function(x, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(x)

  # apply isel to the underlying Variable
  new_var <- isel(x@variable, ...)

  # slice coordinates
  s <- shape(x@variable)
  new_coords <- list()
  for (nm in names(x@coords)) {
    coord <- x@coords[[nm]]
    cdim <- coord_dim(coord)
    if (cdim %in% names(selections)) {
      sel_idx <- selections[[cdim]]
      if (length(sel_idx) == 1L) {
        # dimension is being dropped, don't include coord
        next
      }
      new_coords[[nm]] <- coord_slice(coord, sel_idx)
    } else {
      new_coords[[nm]] <- coord
    }
  }

  DataArray(variable = new_var, coords = new_coords, name = x@name)
}


method(isel, Dataset) <- function(x, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(x)

  new_vars <- lapply(x@data_vars, function(v) {
    # only apply selections for dims this variable has
    v_sels <- selections[names(selections) %in% v@dims]
    if (length(v_sels) == 0L) return(v)
    do.call(isel, c(list(v), v_sels))
  })

  new_coords <- list()
  for (nm in names(x@coords)) {
    coord <- x@coords[[nm]]
    cdim <- coord_dim(coord)
    if (cdim %in% names(selections)) {
      sel_idx <- selections[[cdim]]
      if (length(sel_idx) == 1L) next
      new_coords[[nm]] <- coord_slice(coord, sel_idx)
    } else {
      new_coords[[nm]] <- coord
    }
  }

  Dataset(data_vars = new_vars, coords = new_coords, attrs = x@attrs)
}


# --- sel: label-based indexing ---

#' @rdname indexing
#' @export
sel <- new_generic("sel", "x")

method(sel, DataArray) <- function(x, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(x)

  # convert coordinate values to integer indices
  int_sels <- list()
  for (d in names(selections)) {
    val <- selections[[d]]
    # find the coord for this dim
    coord <- NULL
    for (c in x@coords) {
      if (coord_dim(c) == d) { coord <- c; break }
    }
    if (is.null(coord)) {
      stop(sprintf("no coordinate found for dimension '%s'", d))
    }

    if (length(val) == 2L && is.numeric(val)) {
      # range selection: find all indices between val[1] and val[2]
      all_vals <- coord_values(coord)
      mask <- all_vals >= min(val) & all_vals <= max(val)
      int_sels[[d]] <- which(mask)
    } else if (length(val) == 1L) {
      # single value: nearest lookup
      int_sels[[d]] <- coord_lookup(coord, val)
    } else {
      # multiple specific values
      int_sels[[d]] <- coord_lookup(coord, val)
    }
  }

  do.call(isel, c(list(x), int_sels))
}


method(sel, Dataset) <- function(x, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(x)

  # convert to integer indices using dataset coords
  int_sels <- list()
  for (d in names(selections)) {
    val <- selections[[d]]
    coord <- NULL
    for (c in x@coords) {
      if (coord_dim(c) == d) { coord <- c; break }
    }
    if (is.null(coord))
      stop(sprintf("no coordinate found for dimension '%s'", d))

    if (length(val) == 2L && is.numeric(val)) {
      all_vals <- coord_values(coord)
      mask <- all_vals >= min(val) & all_vals <= max(val)
      int_sels[[d]] <- which(mask)
    } else if (length(val) == 1L) {
      int_sels[[d]] <- coord_lookup(coord, val)
    } else {
      int_sels[[d]] <- coord_lookup(coord, val)
    }
  }

  do.call(isel, c(list(x), int_sels))
}
