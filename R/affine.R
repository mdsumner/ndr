#' Affine (geotransform) index
#'
#' An `AffineIndex` owns two dimensions at once, the columns (`dims[1]`,
#' usually x or lon) and rows (`dims[2]`, usually y or lat) of a raster
#' grid, and maps positions to coordinates with the six numbers of a GDAL
#' geotransform. Coordinates are never stored: they are computed from the
#' transform when asked for, as in xarray's `rasterix.RasterIndex`.
#'
#' The transform is in GDAL order, for pixel corners:
#' `x = t[1] + col * t[2] + row * t[3]`,
#' `y = t[4] + col * t[5] + row * t[6]`,
#' so `t[3]` and `t[6]` are the rotation terms. Coordinates reported by
#' the index are pixel centres.
#'
#' Selecting a regular (evenly spaced) subset keeps the index affine, with
#' the transform moved and scaled to match. An irregular subset of a
#' north-up grid becomes two one-dimensional coordinates. The CRS travels
#' with the index (one place, not two), and two indexes are only equal
#' when their CRS agrees as well as their grid.
#'
#' @param dims Character, length 2: the column dim then the row dim.
#' @param shape Integer, length 2: number of columns, number of rows.
#' @param transform Double, length 6: GDAL geotransform.
#' @param crs Character: CRS as WKT, PROJJSON or an authority code
#'   (length 0 when unknown).
#'
#' @examples
#' ix <- AffineIndex(
#'   dims = c("x", "y"), shape = c(360L, 180L),
#'   transform = c(-180, 1, 0, 90, 0, -1), crs = "EPSG:4326"
#' )
#' index_sel(ix, list(x = c(140, 150), y = c(-45, -40)))
#' as_geotransform(index_isel(ix, list(x = 321:330, y = 131:135))[[1]])
#'
#' @export
AffineIndex <- new_class("AffineIndex",
  parent = Index,
  properties = list(
    dims      = class_character,
    shape     = class_integer,
    transform = class_double,
    crs       = new_property(class_character, default = character())
  ),
  validator = function(self) {
    if (length(self@dims) != 2L || anyDuplicated(self@dims))
      return("dims must be two distinct dimension names")
    if (length(self@shape) != 2L || anyNA(self@shape) || any(self@shape < 0L))
      return("shape must be two non-negative integers")
    if (length(self@transform) != 6L || anyNA(self@transform))
      return("transform must be 6 numbers (a GDAL geotransform)")
    if (affine_det(self@transform) == 0)
      return("transform is singular (zero pixel size)")
    if (length(self@crs) > 1L) return("crs must be length 0 or 1")
    NULL
  }
)


#' Get the GDAL geotransform of an AffineIndex
#'
#' @param x An [AffineIndex]
#' @return Named double vector of length 6, in GDAL order
#' @export
as_geotransform <- function(x) {
  if (!S7_inherits(x, AffineIndex)) stop("x must be an AffineIndex", call. = FALSE)
  stats::setNames(x@transform, c("origin_x", "pixel_width", "row_rotation",
                                 "origin_y", "column_rotation", "pixel_height"))
}


#' Combine two regular coordinates into an AffineIndex
#'
#' Replaces the two [ImplicitCoord]s for `dims` with one [AffineIndex], so
#' the grid and its CRS are carried, selected and compared as one thing.
#'
#' @param x A DataArray or Dataset
#' @param dims Character, length 2: the column (x) dim then the row (y) dim.
#' @param crs Optional CRS string to attach.
#' @return `x` with its coords updated
#'
#' @examples
#' v <- Variable(dims = c("lon", "lat"), data = matrix(1:12, 4, 3))
#' da <- DataArray(variable = v, coords = list(
#'   lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100.5, step = 1),
#'   lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -40.5, step = -1)
#' ))
#' set_affine_index(da, c("lon", "lat"), crs = "EPSG:4326")
#' @export
set_affine_index <- function(x, dims = c("x", "y"), crs = character()) {
  coords <- x@coords
  nx <- find_index(coords, dims[1L])
  ny <- find_index(coords, dims[2L])
  if (is.null(nx) || is.null(ny)) {
    stop(sprintf("no coordinates found for dims '%s' and '%s'", dims[1L], dims[2L]),
         call. = FALSE)
  }
  ix <- affine_from_coords(coords[[nx]], coords[[ny]], crs)
  if (is.null(ix)) {
    stop("an AffineIndex needs two regular (ImplicitCoord) coordinates", call. = FALSE)
  }
  coords[c(nx, ny)] <- NULL
  coords <- c(stats::setNames(list(ix), paste(dims, collapse = ",")), coords)
  x@coords <- coords
  x
}


# --- internals ---

affine_det <- function(t) t[2L] * t[6L] - t[3L] * t[5L]

is_rectilinear <- function(x) x@transform[3L] == 0 && x@transform[5L] == 0

#' Pixel-centre coordinates of 1-based (col, row) positions
#' @noRd
affine_forward <- function(t, i, j) {
  list(
    x = t[1L] + (i - 0.5) * t[2L] + (j - 0.5) * t[3L],
    y = t[4L] + (i - 0.5) * t[5L] + (j - 0.5) * t[6L]
  )
}

#' Continuous (0-based, pixel-edge) column and row of coordinates
#' @noRd
affine_reverse <- function(t, x, y) {
  det <- affine_det(t)
  dx <- x - t[1L]
  dy <- y - t[4L]
  list(
    col = (t[6L] * dx - t[3L] * dy) / det,
    row = (-t[5L] * dx + t[2L] * dy) / det
  )
}

#' One axis of a north-up grid as an ImplicitCoord
#' @noRd
affine_axis <- function(x, which) {
  t <- x@transform
  if (which == 1L) {
    ImplicitCoord(dimension = x@dims[1L], n = x@shape[1L],
                  offset = t[1L] + 0.5 * t[2L], step = t[2L])
  } else {
    ImplicitCoord(dimension = x@dims[2L], n = x@shape[2L],
                  offset = t[4L] + 0.5 * t[6L], step = t[6L])
  }
}

#' AffineIndex from two ImplicitCoords (NULL if either is not regular)
#' @noRd
affine_from_coords <- function(xc, yc, crs = character()) {
  if (!S7_inherits(xc, ImplicitCoord) || !S7_inherits(yc, ImplicitCoord)) return(NULL)
  if (xc@n < 1L || yc@n < 1L || xc@step == 0 || yc@step == 0) return(NULL)
  crs <- crs[!is.na(crs) & nzchar(crs)]
  AffineIndex(
    dims = c(xc@dimension, yc@dimension),
    shape = c(xc@n, yc@n),
    transform = c(xc@offset - 0.5 * xc@step, xc@step, 0,
                  yc@offset - 0.5 * yc@step, 0, yc@step),
    crs = as.character(crs)
  )
}

is_regular_index <- function(idx) {
  length(idx) <= 1L || (all(diff(idx) == diff(idx)[1L]) && diff(idx)[1L] != 0)
}

format_affine_summary <- function(x) {
  t <- x@transform
  rot <- if (is_rectilinear(x)) "" else ", rotated"
  sprintf("(%s) affine %d x %d, res %g, %g%s%s",
          paste(x@dims, collapse = ", "), x@shape[1L], x@shape[2L],
          t[2L], t[6L], rot,
          if (length(x@crs)) paste0(", crs ", crs_label(x@crs)) else "")
}

#' Short label for a CRS string: the authority code if there is one
#' @noRd
crs_label <- function(crs) {
  m <- regmatches(crs, regexpr('ID\\["[A-Za-z]+",[0-9]+\\]\\]?$', crs))
  if (length(m) == 1L) {
    return(sub('ID\\["([A-Za-z]+)",([0-9]+)\\]\\]?$', "\\1:\\2", m))
  }
  nm <- regmatches(crs, regexpr('^[A-Z]+\\["[^"]*"', crs))
  if (length(nm) == 1L) return(sub('^[A-Z]+\\["', "", sub('"$', "", nm)))
  if (nchar(crs) > 40L) paste0(substr(crs, 1L, 37L), "...") else crs
}


# --- Index contract ---

method(index_dims, AffineIndex) <- function(x) x@dims

method(index_sizes, AffineIndex) <- function(x) stats::setNames(x@shape, x@dims)

method(index_coords, AffineIndex) <- function(x) {
  if (is_rectilinear(x)) {
    return(stats::setNames(
      list(coord_values(affine_axis(x, 1L)), coord_values(affine_axis(x, 2L))),
      x@dims
    ))
  }
  ij <- expand.grid(i = seq_len(x@shape[1L]), j = seq_len(x@shape[2L]))
  xy <- affine_forward(x@transform, ij$i, ij$j)
  stats::setNames(
    list(array(xy$x, x@shape), array(xy$y, x@shape)),
    x@dims
  )
}

method(index_sel, AffineIndex) <- function(x, labels) {
  if (is_rectilinear(x)) {
    out <- list()
    for (k in 1:2) {
      d <- x@dims[k]
      if (!is.null(labels[[d]])) {
        out <- c(out, index_sel(affine_axis(x, k), labels[d]))
      }
    }
    return(out)
  }
  lx <- labels[[x@dims[1L]]]
  ly <- labels[[x@dims[2L]]]
  if (is.null(lx) || is.null(ly)) {
    stop(sprintf("selecting on a rotated grid needs both '%s' and '%s'",
                 x@dims[1L], x@dims[2L]), call. = FALSE)
  }
  t <- x@transform
  clamp <- function(v, n) pmax(1L, pmin(as.integer(v), n))
  if (length(lx) == 2L && length(ly) == 2L) {
    # bounding range of the pixels under the four corners
    corners <- expand.grid(x = lx, y = ly)
    cr <- affine_reverse(t, corners$x, corners$y)
    cols <- clamp(floor(range(cr$col)) + 1, x@shape[1L])
    rows <- clamp(floor(range(cr$row)) + 1, x@shape[2L])
    return(stats::setNames(list(seq(cols[1L], cols[2L]), seq(rows[1L], rows[2L])),
                           x@dims))
  }
  if (length(lx) == 1L && length(ly) == 1L) {
    cr <- affine_reverse(t, lx, ly)
    return(stats::setNames(
      list(clamp(floor(cr$col) + 1, x@shape[1L]), clamp(floor(cr$row) + 1, x@shape[2L])),
      x@dims
    ))
  }
  stop("on a rotated grid, select a single point or an x/y range", call. = FALSE)
}

method(index_isel, AffineIndex) <- function(x, positions) {
  xd <- x@dims[1L]
  yd <- x@dims[2L]
  pi <- positions[[xd]]
  pj <- positions[[yd]]
  drop_i <- !is.null(pi) && length(pi) == 1L
  drop_j <- !is.null(pj) && length(pj) == 1L
  if (is.null(pi)) pi <- seq_len(x@shape[1L])
  if (is.null(pj)) pj <- seq_len(x@shape[2L])
  t <- x@transform

  if (drop_i && drop_j) return(list())
  if (drop_i || drop_j) {
    # one dim left: a 1D coordinate along it
    keep <- if (drop_i) 2L else 1L
    if (is_rectilinear(x)) {
      ax <- affine_axis(x, keep)
      return(list(coord_slice(ax, if (keep == 1L) pi else pj)))
    }
    xy <- affine_forward(t, pi, pj)
    return(list(ExplicitCoord(dimension = x@dims[keep],
                              values = xy[[c("x", "y")[keep]]])))
  }

  if (is_regular_index(pi) && is_regular_index(pj) &&
      length(pi) > 0L && length(pj) > 0L) {
    si <- if (length(pi) > 1L) diff(pi)[1L] else 1L
    sj <- if (length(pj) > 1L) diff(pj)[1L] else 1L
    a <- t[2L] * si; b <- t[3L] * sj
    d <- t[5L] * si; e <- t[6L] * sj
    c0 <- affine_forward(t, pi[1L], pj[1L])
    new_t <- c(c0$x - 0.5 * (a + b), a, b, c0$y - 0.5 * (d + e), d, e)
    return(list(AffineIndex(dims = x@dims, shape = c(length(pi), length(pj)),
                            transform = new_t, crs = x@crs)))
  }
  if (length(pi) == 0L || length(pj) == 0L) {
    return(list(AffineIndex(dims = x@dims, shape = c(length(pi), length(pj)),
                            transform = t, crs = x@crs)))
  }
  if (is_rectilinear(x)) {
    return(stats::setNames(
      list(coord_slice(affine_axis(x, 1L), pi), coord_slice(affine_axis(x, 2L), pj)),
      x@dims
    ))
  }
  warning("irregular selection on a rotated grid: coordinates dropped", call. = FALSE)
  list()
}

method(index_drop, AffineIndex) <- function(x, dims) {
  keep <- which(!x@dims %in% dims)
  if (length(keep) != 1L || !is_rectilinear(x)) return(list())
  list(affine_axis(x, keep))
}

method(index_equals, list(AffineIndex, AffineIndex)) <- function(x, y) {
  if (!identical(x@dims, y@dims) || !identical(x@shape, y@shape)) return(FALSE)
  if (!isTRUE(all.equal(x@transform, y@transform, tolerance = 1e-9))) return(FALSE)
  # an unknown CRS matches anything; two known ones must agree
  if (length(x@crs) && length(y@crs) && !identical(x@crs, y@crs)) return(FALSE)
  TRUE
}
