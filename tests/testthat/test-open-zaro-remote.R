## The cheap parts of inst/examples/zaro-engine-parity.R: open public stores
## with the zaro engine (metadata and coordinates only, plus one small read).
## The pixel-for-pixel comparison with GDAL is the example script.

skip_on_cran()
skip_if_offline()
skip_if_not_installed("zaro")
skip_if_not_installed("altarr")

test_that("AODN sea level anomaly opens lazily with decoded time", {
  old <- options(zaro.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  ds <- open_dataset(
    "s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr",
    vars = "GSLA", anonymous = TRUE, region = "ap-southeast-2")
  expect_identical(ds@.backend$engine, "zaro")
  d <- ds_dims(ds)
  expect_equal(unname(d[c("LONGITUDE", "LATITUDE")]), c(641L, 351L))
  expect_s3_class(coord_values(ds@coords$TIME), "Date")
  expect_true(is_lazy(ds$GSLA@variable@data))
})

test_that("CMIP6 int64 time decodes to the CF noleap dates", {
  old <- options(zaro.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  ds <- open_dataset(
    "gs://cmip6/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/historical/r1i1p1f1/Amon/tas/gr1/v20190726",
    vars = character())
  tt <- coord_values(ds@coords$time)
  expect_length(tt, 1980L)
  expect_equal(format(tt[c(1L, 1980L)]), c("1850-01-16", "2014-12-16"))
  expect_equal(c(coord_length(ds@coords$lon), coord_length(ds@coords$lat)),
               c(288L, 180L))
})

test_that("BRAN2023 virtual store: packed temp decodes at a track point", {
  old <- options(zaro.verbose = FALSE)
  on.exit(options(old), add = TRUE)
  ref <- utils::read.csv(system.file("examples", "bran-track-reference.csv",
                                     package = "ndr"))
  ds <- open_dataset(paste0(
    "virtualizarr://https://raw.githubusercontent.com/mdsumner/virtualized/",
    "refs/heads/main/remote/ocean_temp_2023.parq"), vars = "temp")
  x <- ds$temp@variable@data
  r <- ref[1L, ]
  expect_equal(x[cbind(r$i, r$j, r$k, r$l)], r$temp, tolerance = 1e-12)
})
