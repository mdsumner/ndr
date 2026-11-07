make_grid <- function(nx = 6L, ny = 4L) {
  v <- Variable(dims = c("x", "y"), data = array(seq_len(nx * ny), c(nx, ny)))
  DataArray(variable = v, coords = list(
    x = ImplicitCoord(dimension = "x", n = nx, offset = 0.5, step = 1),
    y = ImplicitCoord(dimension = "y", n = ny, offset = 3.5, step = -1)
  ))
}

# --- the contract on 1D coordinates ---

test_that("1D coordinates implement the Index contract", {
  lat <- ImplicitCoord(dimension = "lat", n = 5L, offset = -2, step = 1)
  expect_true(S7_inherits(lat, Index))
  expect_equal(index_dims(lat), "lat")
  expect_equal(index_sizes(lat), c(lat = 5L))
  expect_equal(index_coords(lat), list(lat = c(-2, -1, 0, 1, 2)))
  expect_equal(index_sel(lat, list(lat = c(-1, 1))), list(lat = 2:4))
  expect_equal(index_sel(lat, list(lat = 0.2)), list(lat = 3L))
  expect_length(index_isel(lat, list(lat = 2L)), 0L)
  expect_equal(index_isel(lat, list(lat = 2:3))[[1]]@offset, -1)
  expect_length(index_drop(lat, "lat"), 0L)
})

test_that("index_equals compares labels, across index types", {
  a <- ImplicitCoord(dimension = "lat", n = 3L, offset = 0, step = 0.1)
  b <- ImplicitCoord(dimension = "lat", n = 3L, offset = 0, step = 0.1 + 1e-14)
  e <- ExplicitCoord(dimension = "lat", values = c(0, 0.1, 0.2))
  expect_true(index_equals(a, b))
  expect_true(index_equals(a, e))
  expect_false(index_equals(a, ImplicitCoord(dimension = "lat", n = 3L, offset = 1, step = 0.1)))
  expect_false(index_equals(a, ImplicitCoord(dimension = "lon", n = 3L, offset = 0, step = 0.1)))
})

# --- alignment ---

test_that("arithmetic on mismatched grids is an error by default", {
  a <- make_grid()
  b <- make_grid()
  b@coords$x <- ImplicitCoord(dimension = "x", n = 6L, offset = 1.5, step = 1)
  expect_error(a + b, "not aligned")
  expect_s3_class(a + make_grid(), "ndr::DataArray")
})

test_that("ndr.join = 'override' keeps the left coordinates", {
  a <- make_grid()
  b <- make_grid()
  b@coords$x <- ImplicitCoord(dimension = "x", n = 6L, offset = 1.5, step = 1)
  old <- options(ndr.join = "override")
  on.exit(options(old))
  r <- a + b
  expect_equal(r@coords$x@offset, 0.5)
})

test_that("an operand without coords for a dim aligns with anything", {
  a <- make_grid()
  v <- Variable(dims = "y", data = array(1:4))
  r <- a + DataArray(variable = v)
  expect_equal(names(r@coords), c("x", "y"))
})

# --- AffineIndex ---

test_that("AffineIndex from two regular coords matches them", {
  da <- make_grid()
  af <- set_affine_index(da, c("x", "y"), crs = "EPSG:4326")
  expect_equal(names(af@coords), "x,y")
  ix <- af@coords[[1]]
  expect_equal(unname(as_geotransform(ix)), c(0, 1, 0, 4, 0, -1))
  expect_equal(index_coords(ix), index_coords(da@coords$x) |>
                 c(index_coords(da@coords$y)))
  # selection is the same as with the two 1D coords
  s1 <- sel(da, x = c(1, 3), y = c(1, 2.5))
  s2 <- sel(af, x = c(1, 3), y = c(1, 2.5))
  expect_equal(var_values(s1@variable), var_values(s2@variable))
  expect_equal(sel(af, x = 4.2, y = 0.6)@variable@data, sel(da, x = 4.2, y = 0.6)@variable@data)
})

test_that("regular isel keeps the index affine, with centres preserved", {
  ix <- AffineIndex(dims = c("x", "y"), shape = c(10L, 8L),
                    transform = c(100, 2, 0, 50, 0, -2), crs = "EPSG:3857")
  sub <- index_isel(ix, list(x = c(3L, 5L, 7L), y = 2:4))[[1]]
  expect_true(S7_inherits(sub, AffineIndex))
  expect_equal(sub@shape, c(3L, 3L))
  expect_equal(index_coords(sub)$x, index_coords(ix)$x[c(3, 5, 7)])
  expect_equal(index_coords(sub)$y, index_coords(ix)$y[2:4])
  expect_equal(sub@crs, "EPSG:3857")
})

test_that("irregular isel of a north-up grid gives 1D coords", {
  ix <- AffineIndex(dims = c("x", "y"), shape = c(10L, 8L),
                    transform = c(100, 2, 0, 50, 0, -2))
  out <- index_isel(ix, list(x = c(1L, 2L, 9L)))
  expect_equal(names(out), c("x", "y"))
  expect_true(S7_inherits(out$x, ExplicitCoord))
  expect_true(S7_inherits(out$y, ImplicitCoord))
  expect_equal(out$x@values, c(101, 103, 117))
})

test_that("dropping or reducing one dim leaves the other as a coordinate", {
  af <- set_affine_index(make_grid(), c("x", "y"))
  r <- isel(af, y = 2L)
  expect_equal(names(r@coords), "x,y")
  expect_true(S7_inherits(r@coords[[1]], ImplicitCoord))
  expect_equal(index_dims(r@coords[[1]]), "x")
  m <- nd_mean(af, "x")
  expect_equal(index_dims(m@coords[[1]]), "y")
  expect_equal(coord_values(m@coords[[1]]), c(3.5, 2.5, 1.5, 0.5))
})

test_that("rotated grids select by point and by range", {
  # 45 degree rotation, unit pixels
  r <- sqrt(0.5)
  ix <- AffineIndex(dims = c("x", "y"), shape = c(4L, 4L),
                    transform = c(0, r, r, 0, -r, r))
  centre <- lapply(affine_forward(ix@transform, 3, 2), unname)
  expect_equal(index_sel(ix, list(x = centre$x, y = centre$y)), list(x = 3L, y = 2L))
  expect_error(index_sel(ix, list(x = 1)), "needs both")
  rng <- index_sel(ix, list(x = c(0, 1), y = c(-0.5, 0.5)))
  expect_true(all(lengths(rng) >= 1L))
  co <- index_coords(ix)
  expect_equal(dim(co$x), c(4L, 4L))
})

test_that("AffineIndex equality needs matching grid and CRS", {
  a <- AffineIndex(dims = c("x", "y"), shape = c(2L, 2L),
                   transform = c(0, 1, 0, 2, 0, -1), crs = "EPSG:4326")
  expect_true(index_equals(a, a))
  b <- a
  b@crs <- "EPSG:3857"
  expect_false(index_equals(a, b))
  b@crs <- character()
  expect_true(index_equals(a, b))
  b@transform <- c(0.5, 1, 0, 2, 0, -1)
  expect_false(index_equals(a, b))
})

test_that("AffineIndex validates its inputs", {
  expect_error(AffineIndex(dims = "x", shape = c(1L, 1L), transform = c(0, 1, 0, 0, 0, 1)))
  expect_error(AffineIndex(dims = c("x", "y"), shape = c(1L, 1L), transform = c(0, 0, 0, 0, 0, 1)),
               "singular")
})

test_that("as.data.frame uses affine coordinates", {
  af <- set_affine_index(make_grid(2L, 2L), c("x", "y"))
  df <- as.data.frame(af)
  expect_equal(df$x, c(0.5, 1.5, 0.5, 1.5))
  expect_equal(df$y, c(3.5, 3.5, 2.5, 2.5))
})

# --- GDAL-backed ---

test_that("open_raster carries the geotransform and CRS as an AffineIndex", {
  skip_if_not_installed("GDAL7")
  skip_if_not_installed("altarr")
  tif <- system.file("extdata/test.tif", package = "GDAL7")
  skip_if(!nzchar(tif))
  r <- open_raster(tif)
  ix <- r@coords[[1]]
  expect_true(S7_inherits(ix, AffineIndex))
  expect_equal(unname(as_geotransform(ix)), c(-180, 18, 0, 90, 0, -18))
  expect_true(length(ix@crs) == 1L)
  s <- sel(r, x = c(-90, 0), y = c(0, 45))
  expect_equal(unname(shape(s)), c(5L, 3L))
  expect_equal(unname(as_geotransform(s@coords[[1]])), c(-90, 18, 0, 54, 0, -18))
})

test_that("open_dataset makes an AffineIndex for a georeferenced x/y grid", {
  skip_if_not_installed("GDAL7")
  skip_if_not_installed("altarr")
  tif <- system.file("extdata/test.tif", package = "GDAL7")
  skip_if(!nzchar(tif))
  nc <- tempfile(fileext = ".nc")
  on.exit(unlink(nc))
  made <- tryCatch({
    d <- GDAL7::gdal_create_copy(tif, nc, driver = "netCDF", progress = FALSE)
    GDAL7::gdal_close(d)
    TRUE
  }, error = function(e) FALSE)
  skip_if(!made, "GDAL netCDF driver not available")
  ds <- open_dataset(nc)
  expect_equal(names(ds@coords), "lon,lat")
  ix <- ds@coords[[1]]
  expect_true(S7_inherits(ix, AffineIndex))
  # same cells, same values, as the GeoTIFF it came from
  b <- collect(sel(ds$Band1, lon = c(-90, 0), lat = c(0, 45)))
  r <- collect(sel(open_raster(tif), x = c(-90, 0), y = c(0, 45)))
  expect_equal(sum(var_values(b@variable)), sum(var_values(r@variable)))
})
