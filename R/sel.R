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

  # slice coordinates
  s <- shape(.data@variable)
  new_coords <- list()
  for (nm in names(.data@coords)) {
    coord <- .data@coords[[nm]]
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

  new_coords <- list()
  for (nm in names(.data@coords)) {
    coord <- .data@coords[[nm]]
    cdim <- coord_dim(coord)
    if (cdim %in% names(selections)) {
      sel_idx <- selections[[cdim]]
      if (length(sel_idx) == 1L) next
      new_coords[[nm]] <- coord_slice(coord, sel_idx)
    } else {
      new_coords[[nm]] <- coord
    }
  }

  Dataset(data_vars = new_vars, coords = new_coords, attrs = .data@attrs)
}


# --- sel: label-based indexing ---

#' @rdname indexing
#' @export
sel <- new_generic("sel", ".data")

method(sel, DataArray) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  # convert coordinate values to integer indices
  int_sels <- list()
  for (d in names(selections)) {
    val <- selections[[d]]
    # find the coord for this dim
    coord <- NULL
    for (c in .data@coords) {
      if (coord_dim(c) == d) { coord <- c; break }
    }
    if (is.null(coord)) {
      stop(sprintf("no coordinate found for dimension '%s'", d))
    }

    if (length(val) == 2L && is_orderable(val)) {
      # range selection: find all indices between val[1] and val[2]
      all_vals <- coord_values(coord)
      lo <- min(val)
      hi <- max(val)
      mask <- all_vals >= lo & all_vals <= hi
      int_sels[[d]] <- which(mask)
    } else if (length(val) == 1L) {
      # single value: nearest lookup
      int_sels[[d]] <- coord_lookup(coord, val)
    } else {
      # multiple specific values
      int_sels[[d]] <- coord_lookup(coord, val)
    }
  }

  do.call(isel, c(list(.data), int_sels))
}


# --- LazyDataArray methods: accumulate selections without reading ---

method(isel, LazyDataArray) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  new_selection <- .data@.selection

  for (d in names(selections)) {
    if (!d %in% .data@dims) {
      stop(sprintf("dimension '%s' not found (available: %s)",
                   d, paste(.data@dims, collapse = ", ")))
    }
    val <- as.integer(selections[[d]])

    prev <- new_selection[[d]]
    if (is.null(prev)) {
      # First selection on this dimension: store directly
      new_selection[[d]] <- val
    } else {
      # Compose: previous selection already narrowed the indices.
      # New indices are relative to the already-narrowed dimension,
      # so translate: new absolute indices = prev[val]
      new_selection[[d]] <- prev[val]
    }
  }

  LazyDataArray(
    name       = .data@name,
    dims       = .data@dims,
    dim_sizes  = .data@dim_sizes,
    coords     = .data@coords,
    attrs      = .data@attrs,
    .selection = new_selection,
    .backend   = .data@.backend
  )
}


method(sel, LazyDataArray) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  # Convert coordinate values to integer indices
  int_sels <- list()
  for (d in names(selections)) {
    val <- selections[[d]]
    coord <- NULL
    for (c in .data@coords) {
      if (coord_dim(c) == d) { coord <- c; break }
    }
    if (is.null(coord)) {
      stop(sprintf("no coordinate found for dimension '%s'", d))
    }

    if (length(val) == 2L && is_orderable(val)) {
      # Range selection: find all indices between val[1] and val[2]
      all_vals <- coord_values(coord)
      lo <- min(val)
      hi <- max(val)
      mask <- all_vals >= lo & all_vals <= hi
      int_sels[[d]] <- which(mask)
    } else if (length(val) == 1L) {
      # Single value: nearest lookup
      int_sels[[d]] <- coord_lookup(coord, val)
    } else {
      # Multiple specific values
      int_sels[[d]] <- coord_lookup(coord, val)
    }
  }

  do.call(isel, c(list(.data), int_sels))
}


method(sel, Dataset) <- function(.data, ...) {
  selections <- list(...)
  if (length(selections) == 0L) return(.data)

  # convert to integer indices using dataset coords
  int_sels <- list()
  for (d in names(selections)) {
    val <- selections[[d]]
    coord <- NULL
    for (c in .data@coords) {
      if (coord_dim(c) == d) { coord <- c; break }
    }
    if (is.null(coord))
      stop(sprintf("no coordinate found for dimension '%s'", d))

    if (length(val) == 2L && is_orderable(val)) {
      all_vals <- coord_values(coord)
      lo <- min(val)
      hi <- max(val)
      mask <- all_vals >= lo & all_vals <= hi
      int_sels[[d]] <- which(mask)
    } else if (length(val) == 1L) {
      int_sels[[d]] <- coord_lookup(coord, val)
    } else {
      int_sels[[d]] <- coord_lookup(coord, val)
    }
  }

  do.call(isel, c(list(.data), int_sels))
}
