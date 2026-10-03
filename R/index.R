#' Indexes: the contract between labels and positions
#'
#' An index maps coordinate labels to integer positions along one or more
#' dimensions. It is the object that `sel()` asks "which positions?", that
#' `isel()` asks "what are you after this subset?", and that arithmetic asks
#' "are these two objects on the same grid?". The design follows xarray's
#' Index API (`sel`, `isel`, `equals`, `create_variables`), kept small so
#' that other packages can implement new kinds of index.
#'
#' Every entry in a DataArray or Dataset `coords` list is an `Index`.
#' [ImplicitCoord] and [ExplicitCoord] are one-dimensional indexes;
#' [AffineIndex] owns two dimensions at once through a geotransform.
#'
#' An index implements these generics:
#'
#' - `index_dims(x)`: the dimension names it covers.
#' - `index_sizes(x)`: named integer, the length of each of those dims.
#' - `index_coords(x)`: named list of coordinate values (one per coordinate
#'   it owns; a vector for 1D coordinates, a matrix for 2D).
#' - `index_sel(x, labels)`: `labels` is a named list of label values keyed
#'   by dimension; returns a named list of 1-based integer positions, one
#'   element per dimension that was selected on.
#' - `index_isel(x, positions)`: `positions` is a named list of 1-based
#'   integer positions keyed by dimension (dims not named are kept whole; a
#'   single position drops that dim); returns a list of indexes for the
#'   result (empty when every dim of the index was dropped).
#' - `index_drop(x, dims)`: list of indexes left when `dims` are removed
#'   entirely (by a reduction).
#' - `index_equals(x, y)`: `TRUE` when the two indexes describe the same
#'   labels.
#'
#' @param x,y Index objects
#' @param labels Named list of label values, keyed by dimension name
#' @param positions Named list of 1-based integer positions, keyed by
#'   dimension name
#' @param dims Character vector of dimension names
#'
#' @examples
#' lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1)
#' index_dims(lat)
#' index_sel(lat, list(lat = c(-10, 10)))
#' index_equals(lat, ExplicitCoord(dimension = "lat", values = seq(-89.5, 89.5)))
#'
#' @name indexes
NULL

#' @rdname indexes
#' @export
Index <- new_class("Index", abstract = TRUE)

#' @rdname indexes
#' @export
index_dims <- new_generic("index_dims", "x", function(x) S7_dispatch())

#' @rdname indexes
#' @export
index_sizes <- new_generic("index_sizes", "x", function(x) S7_dispatch())

#' @rdname indexes
#' @export
index_coords <- new_generic("index_coords", "x", function(x) S7_dispatch())

#' @rdname indexes
#' @export
index_sel <- new_generic("index_sel", "x", function(x, labels) S7_dispatch())

#' @rdname indexes
#' @export
index_isel <- new_generic("index_isel", "x", function(x, positions) S7_dispatch())

#' @rdname indexes
#' @export
index_drop <- new_generic("index_drop", "x", function(x, dims) S7_dispatch())

#' @rdname indexes
#' @export
index_equals <- new_generic("index_equals", c("x", "y"), function(x, y) S7_dispatch())

# Fallback: two indexes are equal when they cover the same dims with the
# same sizes and the same coordinate values (so an ImplicitCoord equals an
# ExplicitCoord holding the same regular values).
method(index_equals, list(Index, Index)) <- function(x, y) {
  if (!identical(index_dims(x), index_dims(y))) return(FALSE)
  if (!identical(index_sizes(x), index_sizes(y))) return(FALSE)
  cx <- index_coords(x)
  cy <- index_coords(y)
  if (!identical(names(cx), names(cy))) return(FALSE)
  for (nm in names(cx)) {
    if (!values_equal(cx[[nm]], cy[[nm]])) return(FALSE)
  }
  TRUE
}


# --- helpers used by DataArray / Dataset methods ---

#' Compare coordinate values with a tolerance for floating point
#' @noRd
values_equal <- function(a, b) {
  if (length(a) != length(b)) return(FALSE)
  if (is.numeric(a) && is.numeric(b) && !is.object(a) && !is.object(b)) {
    return(isTRUE(all.equal(as.vector(a), as.vector(b), tolerance = 1e-9,
                            check.attributes = FALSE)))
  }
  if (inherits(a, "POSIXt") && inherits(b, "POSIXt")) {
    return(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
  }
  identical(a, b) || isTRUE(all.equal(a, b, check.attributes = FALSE))
}

#' Name of the first index in `coords` that covers dimension `dim`
#' @noRd
find_index <- function(coords, dim) {
  for (nm in names(coords)) {
    if (dim %in% index_dims(coords[[nm]])) return(nm)
  }
  NULL
}

#' Keep the indexes whose dims are all in `dims`
#' @noRd
indexes_for_dims <- function(coords, dims) {
  Filter(function(ix) all(index_dims(ix) %in% dims), coords)
}

#' Append the indexes `new` (from index_isel / index_drop of the index named
#' `nm`) to `out`. A single result keeps the original name; several are
#' named by their own names (or dims).
#' @noRd
add_indexes <- function(out, nm, new) {
  if (length(new) == 0L) return(out)
  if (length(new) == 1L) {
    out[[nm]] <- new[[1L]]
    return(out)
  }
  nms <- names(new)
  if (is.null(nms)) nms <- vapply(new, function(ix) index_dims(ix)[1L], "")
  for (i in seq_along(new)) out[[nms[i]]] <- new[[i]]
  out
}

#' Apply integer selections to every index in a coords list
#' @noRd
isel_indexes <- function(coords, selections) {
  out <- list()
  for (nm in names(coords)) {
    ix <- coords[[nm]]
    pos <- selections[names(selections) %in% index_dims(ix)]
    if (length(pos) == 0L) {
      out[[nm]] <- ix
    } else {
      out <- add_indexes(out, nm, index_isel(ix, pos))
    }
  }
  out
}

#' Remove dims entirely (reductions) from every index in a coords list
#' @noRd
drop_indexes <- function(coords, dims) {
  out <- list()
  for (nm in names(coords)) {
    ix <- coords[[nm]]
    if (any(index_dims(ix) %in% dims)) {
      out <- add_indexes(out, nm, index_drop(ix, dims))
    } else {
      out[[nm]] <- ix
    }
  }
  out
}

#' Turn label selections into integer selections, grouping the labels by
#' the index that owns each dimension
#' @noRd
sel_positions <- function(coords, selections) {
  owner <- vapply(names(selections), function(d) {
    nm <- find_index(coords, d)
    if (is.null(nm)) stop(sprintf("no coordinate found for dimension '%s'", d),
                          call. = FALSE)
    nm
  }, character(1))
  positions <- list()
  for (nm in unique(owner)) {
    labels <- selections[owner == nm]
    positions <- c(positions, index_sel(coords[[nm]], labels))
  }
  positions
}

#' Check that the indexes of two operands agree, and merge them
#'
#' Indexes that share a dimension must be equal (`join = "exact"`, the
#' default), or the left one is kept (`join = "override"`).
#' @noRd
align_indexes <- function(c1, c2, dims, join = getOption("ndr.join", "exact")) {
  join <- match.arg(join, c("exact", "override"))
  if (join == "exact") {
    for (n1 in names(c1)) {
      d1 <- index_dims(c1[[n1]])
      for (n2 in names(c2)) {
        shared <- intersect(d1, index_dims(c2[[n2]]))
        if (length(shared) == 0L) next
        if (!index_equals(c1[[n1]], c2[[n2]])) {
          stop(sprintf(paste0(
            "operands are not aligned: the indexes for dimension '%s' ",
            "differ ('%s' vs '%s').\n",
            "Select both onto the same grid first, or set ",
            "options(ndr.join = \"override\") to keep the left-hand coordinates."),
            shared[1L], n1, n2), call. = FALSE)
        }
      }
    }
  }
  merged <- c1
  covered <- unlist(lapply(c1, index_dims), use.names = FALSE)
  for (n2 in names(c2)) {
    d2 <- index_dims(c2[[n2]])
    if (!any(d2 %in% covered) && is.null(merged[[n2]])) {
      merged[[n2]] <- c2[[n2]]
      covered <- c(covered, d2)
    }
  }
  indexes_for_dims(merged, dims)
}

#' Validate a coords list against a dims -> size mapping
#' @noRd
check_indexes <- function(coords, sizes, where) {
  for (nm in names(coords)) {
    ix <- coords[[nm]]
    if (!S7_inherits(ix, Index)) {
      return(sprintf("coordinate '%s' is not an Index (ImplicitCoord, ExplicitCoord, AffineIndex, ...)", nm))
    }
    isz <- index_sizes(ix)
    for (d in names(isz)) {
      if (!d %in% names(sizes)) {
        return(sprintf(
          "coordinate '%s' references dim '%s' which is not in the %s (%s)",
          nm, d, where, paste(names(sizes), collapse = ", ")
        ))
      }
      if (isz[[d]] != sizes[[d]]) {
        return(sprintf(
          "coordinate '%s' has length %d but dim '%s' has size %d",
          nm, isz[[d]], d, sizes[[d]]
        ))
      }
    }
  }
  NULL
}
