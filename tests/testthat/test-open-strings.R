## 1D data variables along another dimension, and string variables

## zaro reads string dtypes from 0.0.1.9034 (hypertidy/zaro#27)
skip_if_no_zaro_strings <- function() {
  skip_if_not_installed("zaro")
  skip_if(utils::packageVersion("zaro") < "0.0.1.9034",
          "zaro too old to read string dtypes")
}

## (y = 3, x = 4, band = 2) refl; band coordinate (strings); band_mean (1D on
## band, not named for it); label (strings on band); code (strings, 2D)
make_store <- function(dir) {
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  writeLines('{"zarr_format": 2}', file.path(dir, ".zgroup"))
  writeLines('{}', file.path(dir, ".zattrs"))
  refl <- array(as.double(seq_len(24)), c(3, 4, 2))
  write_v2(file.path(dir, "refl"), refl, c(3, 4, 2), c("y", "x", "band"))
  write_v2(file.path(dir, "y"), c(10, 20, 30), 3, "y")
  write_v2(file.path(dir, "x"), c(1, 2, 3, 4), 4, "x")
  write_v2(file.path(dir, "band"), c("b02", "b04"), 2, "band", dtype = "<U3",
           fill = '""')
  write_v2(file.path(dir, "band_mean"), c(0.5, 0.25), 2, "band")
  write_v2(file.path(dir, "label"), c("blue", "réd"), 2, "band",
           dtype = "<U4", fill = '""')
  code <- matrix(c("a", "b", "c", "d", "e", "f"), 3, 2)   # (y, band)
  write_v2(file.path(dir, "code"), code, c(2, 2), c("y", "band"),
           dtype = "<U1", fill = '""')
  list(dir = dir, refl = refl, code = code)
}

test_that("zaro engine: 1D variables open lazily, small strings eagerly", {
  skip_if_no_zaro_strings()
  skip_if_not_installed("altarr")
  s <- make_store(file.path(tempdir(), "ndr-strings.zarr"))
  ds <- open_dataset(s$dir, engine = "zaro")

  expect_setequal(names(ds@coords), c("y", "x", "band"))
  expect_identical(ds@coords$band@values, c("b02", "b04"))

  bm <- ds$band_mean
  expect_identical(bm@variable@dims, "band")
  expect_equal(as.vector(as.array(bm)), c(0.5, 0.25))
  expect_equal(as.vector(as.array(sel(bm, band = "b04"))), 0.25)

  expect_setequal(names(ds@data_vars), c("label", "code"))
  lab <- ds$label
  expect_identical(lab@variable@dims, "band")
  expect_identical(as.vector(as.array(lab)), c("blue", "réd"))
  cd <- ds$code
  expect_identical(cd@variable@dims, c("y", "band"))
  expect_identical(unname(as.array(cd)), s$code)

  expect_equal(as.vector(as.array(sel(ds$refl, band = "b02", x = 2))),
               s$refl[, 2, 1])
})

test_that("zaro engine: vars = scopes string variables too", {
  skip_if_no_zaro_strings()
  s <- make_store(file.path(tempdir(), "ndr-strings-scope.zarr"))
  ds <- open_dataset(s$dir, vars = "label", engine = "zaro")
  expect_identical(names(ds@data_vars), "label")
  expect_null(ds@.backend)
})

test_that("zaro engine: string variables over the size limit are skipped", {
  skip_if_no_zaro_strings()
  s <- make_store(file.path(tempdir(), "ndr-strings-max.zarr"))
  old <- options(ndr.eager_string_max = 3)
  on.exit(options(old), add = TRUE)
  expect_message(ds <- open_dataset(s$dir, engine = "zaro"), "code")
  expect_identical(names(ds@data_vars), "label")
})

test_that("GDAL engine: 1D variables open, strings skipped with a message", {
  skip_if_not_installed("GDAL7")
  skip_if_not_installed("altarr")
  s <- make_store(file.path(tempdir(), "ndr-strings-gdal.zarr"))
  expect_message(ds <- open_dataset(s$dir, engine = "gdal"),
                 "band, code, label|non-numeric")
  expect_false("band" %in% names(ds@coords))
  expect_equal(as.vector(as.array(ds$band_mean)), c(0.5, 0.25))
})

test_that("grid_mapping names, simple and extended CF forms", {
  expect_identical(grid_mapping_names(list(list(grid_mapping = "crs"),
                                           list(units = "m"))), "crs")
  expect_identical(grid_mapping_names(list(list(
    grid_mapping = "crs_a: x y crs_b: lat lon"))), c("crs_a", "crs_b"))
  expect_identical(grid_mapping_names(list(list())), character())
})
