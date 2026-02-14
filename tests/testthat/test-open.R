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

oisst_dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"

# --- Lazy (default) ---

test_that("open_dataset is lazy by default", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)

  expect_true(S7_inherits(ds, Dataset))

  # data_vars should be empty (all lazy)
  expect_length(ds@data_vars, 0L)

  # backend should have schemas
  expect_false(is.null(ds@.backend))
  expect_true("sst" %in% names(ds@.backend$schemas))

  # coords are still read
  expect_true(all(c("lat", "lon", "time") %in% names(ds@coords)))
})

test_that("lazy $access triggers read and caches", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)

  # Access triggers read
  da <- ds$sst
  expect_true(S7_inherits(da, DataArray))
  expect_true(length(da@variable@data) > 0L)

  # Should be cached in backend
  expect_true(exists("sst", envir = ds@.backend$cache, inherits = FALSE))

  # Second access should use cache (same object)
  da2 <- ds$sst
  expect_identical(da@variable@data, da2@variable@data)
})

test_that("ds_dims includes lazy variable dimensions", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)
  dims <- ds_dims(ds)

  # Should report dims even though no data is loaded
  expect_true("lon" %in% names(dims))
  expect_true("lat" %in% names(dims))
  expect_true("time" %in% names(dims))
})

# --- Scoped (vars specified) ---

test_that("open_dataset with vars scopes to those vars", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn, vars = "sst")

  # sst should be in backend schemas (lazy), not data_vars
  expect_length(ds@data_vars, 0L)
  expect_equal(names(ds@.backend$schemas), "sst")

  # Access triggers read
  da <- ds$sst
  expect_true(S7_inherits(da, DataArray))
})

test_that("vars = character() gives schema only", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn, vars = character())

  expect_length(ds@data_vars, 0L)
  expect_true(is.null(ds@.backend))  # no lazy vars either
  expect_true(length(ds@coords) > 0L)  # coords still present
})

# --- Coordinates ---

test_that("open_dataset produces correct coordinate types", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)

  # lat/lon should be regular → ImplicitCoord
  expect_true(S7_inherits(ds@coords$lat, ImplicitCoord))
  expect_true(S7_inherits(ds@coords$lon, ImplicitCoord))

  # time should be Date values → ExplicitCoord
  expect_true(S7_inherits(ds@coords$time, ExplicitCoord))
  expect_s3_class(coord_values(ds@coords$time), "Date")
})

test_that("open_dataset decodes CF time correctly", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)
  time_vals <- coord_values(ds@coords$time)

  # First time should be 1981-12-01
  expect_equal(time_vals[1], as.Date("1981-12-01"))
})

test_that("open_dataset reads global attributes", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)

  expect_true("title" %in% names(ds@attrs))
  expect_true("Conventions" %in% names(ds@attrs))
})

# --- Error handling ---

test_that("open_dataset rejects invalid var names", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  expect_error(open_dataset(oisst_dsn, vars = "nonexistent"), "not found")
})

test_that("$access of nonexistent var gives informative error", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)
  expect_error(ds$nonexistent, "no variable.*available.*sst")
})

# --- Integration ---

test_that("lazy open_dataset integrates with ndr operations", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")

  ds <- open_dataset(oisst_dsn)

  # sel/isel on lazily-loaded variable
  da <- ds$sst
  da_t1 <- isel(da, time = 1L)
  expect_equal(ndim(da_t1), 2L)

  da_region <- sel(da, lat = c(-10, 10), lon = c(150, 200))
  expect_true(all(dim(da_region) > 0L))
})


# --- Remote kerchunk-parquet ---

test_that("open_dataset reads remote kerchunk-parquet lazily", {
  skip_on_cran()
  skip_if_offline()

  dsn <- 'ZARR:"/vsicurl/https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq"'

  # Default lazy: reads only coords
  ds <- open_dataset(dsn)

  expect_true(S7_inherits(ds, Dataset))
  expect_length(ds@data_vars, 0L)  # nothing loaded
  expect_true("temp" %in% names(ds@.backend$schemas))

  # Coords should be present and time decoded
  expect_true(all(c("Time", "st_ocean", "yt_ocean", "xt_ocean") %in% names(ds@coords)))
  time_vals <- coord_values(ds@coords$Time)
  expect_s3_class(time_vals, "Date")
})
