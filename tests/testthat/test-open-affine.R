## zaro engine: an AffineIndex from GeoZarr spatial:/proj: attributes or a
## CF grid mapping

skip_if_not_installed("zaro")
skip_if_not_installed("altarr")

## a V2 store: band (x, y) 4 x 3, coords x and y, optional spatial_ref
make_geo <- function(path, group_attrs = NULL, band_attrs = NULL,
                     x = c(5, 15, 25, 35), y = c(95, 85, 75), wkt = NULL) {
  unlink(path, recursive = TRUE)
  dir.create(path, recursive = TRUE)
  writeLines('{"zarr_format": 2}', file.path(path, ".zgroup"))
  writeLines(paste0("{", paste(group_attrs, collapse = ", "), "}"),
             file.path(path, ".zattrs"))
  write_v2(file.path(path, "x"), x, chunk = 4L, dims = "x")
  write_v2(file.path(path, "y"), y, chunk = 3L, dims = "y")
  write_v2(file.path(path, "band"), array(as.double(1:12), c(4L, 3L)),
           chunk = c(4L, 3L), dims = c("x", "y"), attrs = band_attrs)
  if (!is.null(wkt)) {
    write_v2(file.path(path, "spatial_ref"), 0, chunk = 1L, dims = "spatial_ref_dim",
             attrs = sprintf('"crs_wkt": "%s"', wkt))
  }
  path
}

geozarr <- c('"spatial:dimensions": ["y", "x"]',
             '"spatial:transform": [10.0, 0.0, 0.0, 0.0, -10.0, 100.0]',
             '"spatial:shape": [3, 4]', '"proj:code": "EPSG:32636"')

test_that("GeoZarr spatial:transform (pixel) becomes an AffineIndex", {
  ## coordinate arrays deliberately wrong: the transform defines the grid
  p <- make_geo(file.path(tempdir(), "geo-pixel.zarr"),
                group_attrs = c(geozarr, '"spatial:registration": "pixel"'),
                x = c(-1, -2, -3, -4), y = c(-1, -2, -3))
  ds <- open_dataset(p, engine = "zaro")
  expect_length(ds@coords, 1L)
  ix <- ds@coords[[1L]]
  expect_true(S7::S7_inherits(ix, AffineIndex))
  expect_equal(ix@dims, c("x", "y"))
  expect_equal(ix@shape, c(4L, 3L))
  expect_equal(unname(as_geotransform(ix)), c(0, 10, 0, 100, 0, -10))
  expect_equal(ix@crs, "EPSG:32636")
  cc <- index_coords(ix)
  expect_equal(as.double(cc$x), c(5, 15, 25, 35))
  expect_equal(as.double(cc$y), c(95, 85, 75))
  sub <- ds$band |> sel(x = c(10, 30), y = c(90, 70)) |> collect()
  expect_equal(as.array(sub), array(as.double(1:12), c(4L, 3L))[2:3, 2:3],
               ignore_attr = TRUE)
})

test_that("node registration moves the transform to the pixel corner", {
  ## the transform locates the first pixel's centre, (5, 95)
  node <- geozarr
  node[2] <- '"spatial:transform": [10.0, 0.0, 5.0, 0.0, -10.0, 95.0]'
  p <- make_geo(file.path(tempdir(), "geo-node.zarr"),
                group_attrs = c(node, '"spatial:registration": "node"'))
  ix <- open_dataset(p, engine = "zaro")@coords[[1L]]
  expect_equal(unname(as_geotransform(ix)), c(0, 10, 0, 100, 0, -10))
})

test_that("the transform can be on the data variable, CRS from grid_mapping", {
  p <- make_geo(file.path(tempdir(), "geo-var.zarr"),
                band_attrs = c(geozarr[1:3], '"grid_mapping": "spatial_ref"'),
                wkt = "LOCAL_CS[\\\"test\\\"]")
  ix <- open_dataset(p, engine = "zaro")@coords[[1L]]
  expect_true(S7::S7_inherits(ix, AffineIndex))
  expect_equal(ix@crs, 'LOCAL_CS["test"]')
})

test_that("a spatial:shape that disagrees is ignored: coords are read", {
  bad <- sub("\\[3, 4\\]", "[30, 40]", geozarr)
  p <- make_geo(file.path(tempdir(), "geo-badshape.zarr"), group_attrs = bad)
  ds <- open_dataset(p, engine = "zaro")
  expect_setequal(names(ds@coords), c("x", "y"))
})

test_that("CF grid mapping: regular coords plus crs_wkt become an AffineIndex", {
  p <- make_geo(file.path(tempdir(), "geo-cf.zarr"),
                band_attrs = '"grid_mapping": "spatial_ref"',
                wkt = "LOCAL_CS[\\\"test\\\"]")
  ds <- open_dataset(p, engine = "zaro")
  ix <- ds@coords[[1L]]
  expect_true(S7::S7_inherits(ix, AffineIndex))
  expect_equal(unname(as_geotransform(ix)), c(0, 10, 0, 100, 0, -10))
  expect_equal(ix@crs, 'LOCAL_CS["test"]')
  expect_false("spatial_ref" %in% names(ds))
})

test_that("no georeferencing: plain coordinates, as before", {
  p <- make_geo(file.path(tempdir(), "geo-none.zarr"))
  ds <- open_dataset(p, engine = "zaro")
  expect_setequal(names(ds@coords), c("x", "y"))
})

## The GDAL engine reads the same attributes: both engines give the same
## AffineIndex (GDAL's own route, CRS plus HORIZONTAL_X/Y dimension types,
## is for formats that declare them, such as netCDF)
same_index <- function(p) {
  z <- open_dataset(p, engine = "zaro")
  g <- open_dataset(p, engine = "gdal")
  expect_identical(names(g@coords), names(z@coords))
  ix_g <- g@coords[[1L]]
  ix_z <- z@coords[[1L]]
  expect_true(S7::S7_inherits(ix_g, AffineIndex))
  expect_equal(ix_g@dims, ix_z@dims)
  expect_equal(ix_g@shape, ix_z@shape)
  expect_equal(as_geotransform(ix_g), as_geotransform(ix_z))
  expect_identical(ix_g@crs, ix_z@crs)
  invisible(g)
}

test_that("GDAL engine: GeoZarr transform, as the zaro engine reads it", {
  skip_if_not_installed("GDAL7")
  p <- make_geo(file.path(tempdir(), "geo-gdal-pixel.zarr"),
                group_attrs = c(geozarr, '"spatial:registration": "pixel"'),
                x = c(-1, -2, -3, -4), y = c(-1, -2, -3))
  g <- same_index(p)
  expect_length(g@coords, 1L)      # the wrong x/y arrays are not read
  sub <- g$band |> sel(x = c(10, 30), y = c(90, 70)) |> collect()
  expect_equal(as.array(sub), array(as.double(1:12), c(4L, 3L))[2:3, 2:3],
               ignore_attr = TRUE)

  node <- geozarr
  node[2] <- '"spatial:transform": [10.0, 0.0, 5.0, 0.0, -10.0, 95.0]'
  same_index(make_geo(file.path(tempdir(), "geo-gdal-node.zarr"),
                      group_attrs = c(node, '"spatial:registration": "node"')))
  same_index(make_geo(file.path(tempdir(), "geo-gdal-var.zarr"),
                      band_attrs = c(geozarr[1:3], '"grid_mapping": "spatial_ref"'),
                      wkt = "LOCAL_CS[\\\"test\\\"]"))
})

test_that("GDAL engine: CF grid mapping, as the zaro engine reads it", {
  skip_if_not_installed("GDAL7")
  g <- same_index(make_geo(file.path(tempdir(), "geo-gdal-cf.zarr"),
                           band_attrs = '"grid_mapping": "spatial_ref"',
                           wkt = "LOCAL_CS[\\\"test\\\"]"))
  expect_false("spatial_ref" %in% names(g))
})
