## open_dataset(engine = "zaro"): no GDAL, a small local V2 store written here

skip_if_not_installed("zaro")
skip_if_not_installed("altarr")


make_zarr_store <- function() {
  path <- file.path(tempdir(), "ndr-zaro-store.zarr")
  unlink(path, recursive = TRUE)
  dir.create(path)
  writeLines('{"zarr_format": 2}', file.path(path, ".zgroup"))
  writeLines('{"title": "zaro engine test"}', file.path(path, ".zattrs"))
  write_v2(file.path(path, "lon"), seq(140.5, by = 1, length.out = 5), 5L, "lon")
  write_v2(file.path(path, "lat"), seq(-45.5, by = 1, length.out = 4), 4L, "lat")
  write_v2(file.path(path, "time"), c(0, 1, 2), 3L, "time",
           attrs = '"units": "days since 2020-01-01"')
  ## packed int16 temp(time, lat, lon): value = raw * 0.5 + 10
  raw <- array(seq_len(5 * 4 * 3), c(5L, 4L, 3L))
  raw[2, 3, 1] <- -32768L
  write_v2(file.path(path, "temp"), raw, c(3L, 2L, 1L),
           c("lon", "lat", "time"), dtype = "<i2", fill = -32768,
           attrs = c('"scale_factor": 0.5', '"add_offset": 10',
                     '"_FillValue": -32768', '"units": "degC"'))
  list(path = path, raw = raw)
}

test_that("guess_engine picks zaro for zaro schemes and objects", {
  expect_equal(guess_engine("s3://bucket/x.zarr"), "zaro")
  expect_equal(guess_engine("virtualizarr://https://x/y.parq"), "zaro")
  expect_equal(guess_engine("mdimrefs:///x.sqlite"), "zaro")
  expect_equal(guess_engine("byterefs:///x.sqlite"), "zaro")
  expect_equal(guess_engine("flatrefs://https://host/flat"), "zaro")
  expect_equal(guess_engine("/vsicurl/https://x/y.nc"), "gdal")
  expect_equal(guess_engine("file.nc"), "gdal")
  expect_equal(guess_engine("/data/sst.zarr"), "zaro")
  expect_equal(guess_engine("https://example.com/store.ZARR/"), "zaro")
  expect_equal(guess_engine('ZARR:"/vsicurl/https://x/y.zarr"'), "gdal")
  expect_equal(guess_engine("/vsis3/bucket/y.zarr"), "gdal")
  expect_equal(guess_engine(structure(list(), class = "zaro_dataset")), "zaro")
})

test_that("a .zarr path opens with the zaro engine by default", {
  s <- make_zarr_store()
  expect_identical(open_dataset(s$path)@.backend$engine, "zaro")
})

test_that("open_dataset(engine = 'zaro') is lazy with decoded coords", {
  s <- make_zarr_store()
  ds <- open_dataset(s$path, engine = "zaro")
  expect_true(S7::S7_inherits(ds, Dataset))
  expect_length(ds@data_vars, 0L)
  expect_equal(names(ds@.backend$schemas), "temp")
  expect_setequal(names(ds@coords), c("lon", "lat", "time"))
  expect_equal(coord_values(ds@coords$time), as.Date("2020-01-01") + 0:2)
  expect_equal(ds@attrs$title, "zaro engine test")
  expect_equal(ds_dims(ds), c(lon = 5L, lat = 4L, time = 3L))
})

test_that("zaro engine data are CF-decoded; packing moves to encoding", {
  s <- make_zarr_store()
  ds <- open_dataset(s$path, engine = "zaro")
  da <- ds$temp
  expect_true(is_lazy(da@variable@data))
  expect_equal(da@variable@dims, c("lon", "lat", "time"))
  expect_equal(da@variable@attrs$units, "degC")
  expect_null(da@variable@attrs$scale_factor)
  expect_equal(da@variable@encoding$scale_factor, 0.5)

  want <- s$raw * 0.5 + 10
  want[s$raw == -32768L] <- NA
  got <- collect(da)@variable@data
  expect_equal(as.vector(got), as.vector(want))

  ## selection by coordinate stays lazy and reads the right values
  v <- da |> sel(time = as.Date("2020-01-03"), lat = -43.5) |> collect()
  expect_equal(as.vector(v@variable@data), as.vector(want[, 3, 3]))

  m <- nd_mean(da, "time")
  expect_equal(as.vector(m@variable@data),
               as.vector(apply(want, c(1, 2), mean)))
})

test_that("vars= scoping works with the zaro engine", {
  s <- make_zarr_store()
  expect_length(open_dataset(s$path, vars = character(), engine = "zaro")@.backend, 0L)
  expect_error(open_dataset(s$path, vars = "nope", engine = "zaro"), "not found")
})
