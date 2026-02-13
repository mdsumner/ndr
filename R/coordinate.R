#' Coordinate classes
#'
#' A Coordinate maps integer indices to meaningful values along a dimension.
#' Two representations:
#'
#' - **ImplicitCoord**: regular grids defined by offset + step. Coordinates are
#'   never stored, computed on demand via `offset + (0:(n-1)) * step`. This is
#'   the raster/terra cell abstraction, and xarray's RangeIndex.
#'
#' - **ExplicitCoord**: an arbitrary vector of coordinate values (numeric,
#'   character, POSIXct, or anything with sensible comparison operators).
#'
#' @name coordinates
NULL

#' Create an implicit (regular-grid) coordinate
#'
#' @param dimension Dimension name (character, length 1)
#' @param n Number of elements (integer, length 1)
#' @param offset Start value (numeric, length 1)
#' @param step Step size (numeric, length 1)
#'
#' @examples
#' lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
#' coord_values(lat)  # -89.5, -88.5, ..., 89.5
#' coord_lookup(lat, 0)  # integer index of latitude 0
#'
#' @export
ImplicitCoord <- new_class("ImplicitCoord",
  properties = list(
    dimension = class_character,
    n      = class_integer,
    offset = class_double,
    step   = class_double
  ),
  validator = function(self) {
    if (length(self@dimension) != 1L) return("dimension must be length 1")
    if (length(self@n) != 1L || self@n < 0L) return("n must be a non-negative scalar integer")
    if (length(self@offset) != 1L) return("offset must be a scalar")
    if (length(self@step) != 1L) return("step must be a scalar")
    if (self@step == 0 && self@n > 1L) return("step cannot be 0 for n > 1")
    NULL
  }
)

#' Create an explicit (irregular) coordinate
#'
#' @param dimension Dimension name (character, length 1)
#' @param values Vector of coordinate values
#'
#' @examples
#' time <- ExplicitCoord(
#'   dimension = "time",
#'   values = as.Date("2020-01-01") + 0:364
#' )
#' coord_length(time)  # 365
#'
#' @export
ExplicitCoord <- new_class("ExplicitCoord",
  properties = list(
    dimension = class_character,
    values = class_any
  ),
  validator = function(self) {
    if (length(self@dimension) != 1L) return("dimension must be length 1")
    NULL
  }
)


# --- Generics that work on both coord types ---

#' Get the values of a coordinate
#' @param x A coordinate (ImplicitCoord or ExplicitCoord)
#' @return Vector of coordinate values
#' @export
coord_values <- new_generic("coord_values", "x")

method(coord_values, ImplicitCoord) <- function(x) {
  if (x@n == 0L) return(numeric(0))
  x@offset + seq.int(0L, x@n - 1L) * x@step
}

method(coord_values, ExplicitCoord) <- function(x) {
  x@values
}

#' Get the length of a coordinate
#' @param x A coordinate
#' @return Integer length
#' @export
coord_length <- new_generic("coord_length", "x")

method(coord_length, ImplicitCoord) <- function(x) x@n
method(coord_length, ExplicitCoord) <- function(x) length(x@values)

#' Look up integer index for a coordinate value
#'
#' Returns the 1-based index of the element nearest to `value`.
#' For ImplicitCoord, this is O(1) arithmetic.
#' For ExplicitCoord on numeric values, binary search if sorted,
#' linear scan otherwise.
#'
#' @param x A coordinate
#' @param value The value(s) to look up
#' @return Integer index (1-based)
#' @export
coord_lookup <- new_generic("coord_lookup", "x")

method(coord_lookup, ImplicitCoord) <- function(x, value) {
  # n=0: no valid indices
  if (x@n == 0L) return(integer(0))
  # n=1: always index 1 (avoids division by zero when step=0)
  if (x@n == 1L) return(rep(1L, length(value)))
  # O(1): invert the affine transform
  continuous_idx <- (value - x@offset) / x@step
  idx <- round(continuous_idx) + 1L  # to 1-based
  # clamp to valid range
  idx <- pmax(1L, pmin(as.integer(idx), x@n))
  idx
}

method(coord_lookup, ExplicitCoord) <- function(x, value) {
  v <- x@values
  if (is.numeric(v)) {
    if (!is.unsorted(v)) {
      # sorted ascending: binary search for nearest
      vapply(value, function(val) {
        pos <- findInterval(val, v)
        if (pos == 0L) return(1L)
        if (pos == length(v)) return(length(v))
        # pick the closer of pos and pos+1
        if (abs(v[pos] - val) <= abs(v[pos + 1L] - val)) pos else pos + 1L
      }, integer(1))
    } else {
      # unsorted (or descending): linear nearest-neighbor
      vapply(value, function(val) {
        which.min(abs(v - val))
      }, integer(1))
    }
  } else {
    # non-numeric (character, Date, factor, etc.): exact match
    match(value, v)
  }
}

#' Slice a coordinate by integer indices
#' @param x A coordinate
#' @param idx Integer indices (1-based)
#' @return A new coordinate of the same type (ImplicitCoord stays implicit
#'   if the slice is a contiguous regular subsequence)
#' @export
coord_slice <- new_generic("coord_slice", "x")

method(coord_slice, ImplicitCoord) <- function(x, idx) {
  if (length(idx) == 0L) {
    return(ImplicitCoord(
      dimension = x@dimension,
      n      = 0L,
      offset = x@offset,
      step   = x@step
    ))
  }
  # check if idx is a regular sequence (contiguous with constant step)
  if (length(idx) <= 1L || all(diff(idx) == diff(idx)[1L])) {
    stride <- if (length(idx) <= 1L) 1L else diff(idx)[1L]
    ImplicitCoord(
      dimension = x@dimension,
      n      = length(idx),
      offset = x@offset + (idx[1L] - 1L) * x@step,
      step   = x@step * stride
    )
  } else {
    # irregular slice — must materialize
    ExplicitCoord(dimension = x@dimension, values = coord_values(x)[idx])
  }
}

method(coord_slice, ExplicitCoord) <- function(x, idx) {
  ExplicitCoord(dimension = x@dimension, values = x@values[idx])
}

#' Get the dimension name of a coordinate
#' @param x A coordinate
#' @return Character scalar
#' @export
coord_dim <- new_generic("coord_dim", "x")

method(coord_dim, ImplicitCoord) <- function(x) x@dimension
method(coord_dim, ExplicitCoord) <- function(x) x@dimension
