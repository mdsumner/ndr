## Tests for LazyDataArray and the lazy sel → collect() pipeline

oisst_dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"

# --- LazyDataArray construction ---

test_that("ds$var returns LazyDataArray for backend variables", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  sst <- ds$sst
  expect_true(S7_inherits(sst, LazyDataArray))
  expect_equal(sst@name, "sst")
  expect_equal(sst@dims, c("lon", "lat", "time"))
  expect_equal(sst@dim_sizes, c(360L, 180L, 494L))

  # All selection slots should be NULL (no selection yet)
  for (s in sst@.selection) {
    expect_null(s)
  }

  # Backend reference
  expect_equal(sst@.backend$dsn, oisst_dsn)
  expect_equal(sst@.backend$var_name, "sst")
})

test_that("ds$var returns LazyDataArray with coords", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  sst <- ds$sst
  expect_true(length(sst@coords) > 0L)
  expect_true("lat" %in% names(sst@coords))
  expect_true("lon" %in% names(sst@coords))
  expect_true("time" %in% names(sst@coords))
})


# --- collect() ---

test_that("collect() on LazyDataArray returns DataArray", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  # Collect a small subset to avoid reading the whole thing
  result <- ds$sst |> isel(lon = 1:3, lat = 1:3, time = 1:2) |> collect()
  expect_true(S7_inherits(result, DataArray))
  expect_equal(result@name, "sst")
  expect_equal(shape(result@variable), c(lon = 3L, lat = 3L, time = 2L))
})

test_that("collect() on DataArray is identity", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  da <- DataArray(variable = v, name = "test")
  expect_identical(collect(da), da)
})

test_that("collect() with no selection reads full variable", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  # Read a single time step (full lon x lat)
  result <- ds$sst |> isel(time = 1L) |> collect()
  expect_true(S7_inherits(result, DataArray))
  expect_equal(shape(result@variable), c(lon = 360L, lat = 180L))
})


# --- Lazy isel ---

test_that("isel on LazyDataArray accumulates selection", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  lazy1 <- ds$sst |> isel(lat = 10:20)
  expect_true(S7_inherits(lazy1, LazyDataArray))
  expect_equal(lazy1@.selection$lat, 10:20)
  expect_null(lazy1@.selection$lon)
  expect_null(lazy1@.selection$time)
})

test_that("chained isel composes indices", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  # First select lat indices 10:20, then from that select indices 3:5
  # Final absolute indices should be 12:14
  lazy <- ds$sst |> isel(lat = 10:20) |> isel(lat = 3:5)
  expect_equal(lazy@.selection$lat, 12:14)
})


# --- Lazy sel ---

test_that("sel on LazyDataArray accumulates selection", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  lazy <- ds$sst |> sel(lat = c(-60, -30))
  expect_true(S7_inherits(lazy, LazyDataArray))
  expect_false(is.null(lazy@.selection$lat))
  expect_null(lazy@.selection$lon)
  expect_null(lazy@.selection$time)
})


# --- Lazy → eager equivalence ---

test_that("lazy sel+collect equals eager sel", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  # Lazy: sel then collect
  lazy_r <- ds$sst |>
    sel(lat = c(-60, -30)) |>
    isel(time = 1L) |>
    collect()

  # Eager: collect then sel
  eager_r <- ds$sst |>
    collect() |>
    sel(lat = c(-60, -30)) |>
    isel(time = 1L)

  expect_identical(lazy_r@variable@data, eager_r@variable@data)
  expect_equal(shape(lazy_r@variable), shape(eager_r@variable))
})

test_that("chained sel+collect equals single sel+collect", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  r1 <- ds$sst |>
    sel(lat = c(-60, -30), time = as.Date("2020-06-01")) |>
    collect()

  r2 <- ds$sst |>
    sel(lat = c(-60, -30)) |>
    sel(time = as.Date("2020-06-01")) |>
    collect()

  expect_identical(r1@variable@data, r2@variable@data)
})


# --- Scalar selection (dimension drop) ---

test_that("scalar isel drops dimension on collect", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  result <- ds$sst |> isel(time = 1L) |> collect()
  expect_equal(result@variable@dims, c("lon", "lat"))
  expect_true(!"time" %in% names(result@coords))
})


# --- Auto-collect ---

test_that("arithmetic on LazyDataArray auto-collects", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  lazy <- ds$sst |> isel(lon = 1:3, lat = 1:3, time = 1L)
  result <- lazy + 273.15
  expect_true(S7_inherits(result, DataArray))

  # Compare with explicit collect
  explicit <- collect(lazy) + 273.15
  expect_equal(result@variable@data, explicit@variable@data)
})

test_that("reduction on LazyDataArray auto-collects", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  result <- ds$sst |>
    isel(lon = 1:5, lat = 1:5, time = 1:10) |>
    nd_mean("time", na.rm = TRUE)

  expect_true(S7_inherits(result, DataArray))
  expect_equal(result@variable@dims, c("lon", "lat"))
})

test_that("as.data.frame on LazyDataArray auto-collects", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  df <- ds$sst |>
    isel(lon = 1:2, lat = 1:2, time = 1L) |>
    as.data.frame()

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4L)
  expect_true("sst" %in% names(df))
})


# --- selection_to_hyperslab (internal) ---

test_that("selection_to_hyperslab reverses R order to GDAL C order", {
  # R order: lon, lat, time
  dims <- c("lon", "lat", "time")
  dim_sizes <- c(360L, 180L, 494L)

  # No selection: full dimensions
  hs <- ndr:::selection_to_hyperslab(dims, dim_sizes, list(lon = NULL, lat = NULL, time = NULL))
  # GDAL C-order: time, lat, lon
  expect_equal(hs$start, c(0L, 0L, 0L))
  expect_equal(hs$count, c(494L, 180L, 360L))

  # Partial selection
  hs2 <- ndr:::selection_to_hyperslab(dims, dim_sizes, list(lon = NULL, lat = 10:20, time = 5L))
  expect_equal(hs2$start, c(4L, 9L, 0L))   # time: 5-1=4, lat: 10-1=9, lon: 0
  expect_equal(hs2$count, c(1L, 11L, 360L)) # time: 1, lat: 11, lon: 360
})


# --- Print ---

test_that("print.LazyDataArray shows selection info", {
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
  ds <- open_dataset(oisst_dsn)

  lazy <- ds$sst |> sel(lat = c(-60, -30))
  out <- capture.output(lazy)

  expect_true(any(grepl("LazyDataArray", out)))
  expect_true(any(grepl("not loaded", out)))
  expect_true(any(grepl("selected", out)))
  expect_true(any(grepl("collect", out)))
})
