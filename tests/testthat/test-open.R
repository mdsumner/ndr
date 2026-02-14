## Tests for open_dataset()
##
## These require gdalraster with multidim API support.
## Run locally with: remotes::install_github("mdsumner/gdalraster@gdalmultidim-api")
##
## Test data sources:
##   - OISST: local NetCDF at /rdsi/PUBLIC/raad/data/...
##   - BRAN2023: remote kerchunk-parquet via /vsicurl/

skip_if_not_installed("gdalraster")
skip_if_not(
  exists("mdim_array_read", envir = asNamespace("gdalraster")),
  "gdalraster multidim API not available"
)

# --- Local NetCDF ---

test_that("open_dataset reads local NetCDF", {
  dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
  skip_if_not(file.exists(dsn), "OISST test data not available")

  ds <- open_dataset(dsn)

  # Should be a Dataset
  expect_true(S7_inherits(ds, Dataset))

  # Should have sst as a data variable
  expect_true("sst" %in% names(ds@data_vars))

  # Should have lat, lon, time as coordinates
  expect_true(all(c("lat", "lon", "time") %in% names(ds@coords)))
})

test_that("open_dataset produces correct coordinate types", {
  dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
  skip_if_not(file.exists(dsn), "OISST test data not available")

  ds <- open_dataset(dsn)

  # lat/lon should be regular → ImplicitCoord
  expect_true(S7_inherits(ds@coords$lat, ImplicitCoord))
  expect_true(S7_inherits(ds@coords$lon, ImplicitCoord))

  # time should be Date values → ExplicitCoord
  expect_true(S7_inherits(ds@coords$time, ExplicitCoord))
  expect_s3_class(coord_values(ds@coords$time), "Date")
})

test_that("open_dataset decodes CF time correctly", {
  dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
  skip_if_not(file.exists(dsn), "OISST test data not available")

  ds <- open_dataset(dsn)
  time_vals <- coord_values(ds@coords$time)

  # First time should be 1981-12-01
  expect_equal(time_vals[1], as.Date("1981-12-01"))
})

test_that("open_dataset reads global attributes", {
  dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
  skip_if_not(file.exists(dsn), "OISST test data not available")

  ds <- open_dataset(dsn)

  expect_true("title" %in% names(ds@attrs))
  expect_true("Conventions" %in% names(ds@attrs))
})

test_that("open_dataset with vars filter", {
  dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
  skip_if_not(file.exists(dsn), "OISST test data not available")

  ds <- open_dataset(dsn, vars = "sst")

  expect_equal(names(ds@data_vars), "sst")
})

test_that("open_dataset rejects invalid var names", {
  dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
  skip_if_not(file.exists(dsn), "OISST test data not available")

  expect_error(open_dataset(dsn, vars = "nonexistent"), "not found")
})

test_that("open_dataset result integrates with ndr operations", {
  dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
  skip_if_not(file.exists(dsn), "OISST test data not available")

  ds <- open_dataset(dsn, vars = "sst")

  # Extract as DataArray
  da <- ds$sst
  expect_true(S7_inherits(da, DataArray))

  # isel should work
  da_t1 <- isel(da, time = 1L)
  expect_equal(ndim(da_t1), 2L)  # lat × lon

  # sel should work with coordinate values
  da_region <- sel(da, lat = c(-10, 10), lon = c(150, 200))
  expect_true(all(dim(da_region) > 0L))
})


# --- Remote kerchunk-parquet ---

test_that("open_dataset reads remote kerchunk-parquet", {
  skip_on_cran()
  skip_if_offline()

  dsn <- 'ZARR:"/vsicurl/https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq"'

  # Only read schema + coords, not the full 12TB
  ds <- open_dataset(dsn, vars = character())

  expect_true(S7_inherits(ds, Dataset))
  expect_true(all(c("Time", "st_ocean", "yt_ocean", "xt_ocean") %in% names(ds@coords)))

  # Should have decoded time
  time_vals <- coord_values(ds@coords$Time)
  expect_s3_class(time_vals, "Date")
  expect_equal(time_vals[1], as.Date("2010-01-01"))
})
