test_that("isel on Variable works", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:12, 3, 4))

  # slice keeping dims
  r <- isel(v, x = 1:2)
  expect_equal(r@dims, c("x", "y"))
  expect_equal(unname(shape(r)), c(2L, 4L))

  # scalar drops dim
  r2 <- isel(v, x = 2)
  expect_equal(r2@dims, "y")
  expect_equal(unname(shape(r2)), 4L)
  expect_equal(as.array(r2), array(c(2, 5, 8, 11), dim = 4L))
})

test_that("isel on DataArray slices coordinates", {
  v <- Variable(dims = c("lat", "lon"), data = matrix(1:12, 3, 4))
  da <- DataArray(
    variable = v,
    coords = list(
      lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10),
      lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100, step = 10)
    )
  )

  r <- isel(da, lat = 1:2)
  expect_equal(length(r@coords), 2L)
  expect_equal(coord_length(r@coords$lat), 2L)
  expect_equal(coord_values(r@coords$lat), c(-10, 0))

  # scalar drops coord
  r2 <- isel(da, lat = 1)
  expect_false("lat" %in% names(r2@coords))
  expect_equal(r2@variable@dims, "lon")
})

test_that("sel on DataArray with ImplicitCoord", {
  v <- Variable(dims = c("lat", "lon"), data = matrix(1:12, 3, 4))
  da <- DataArray(
    variable = v,
    coords = list(
      lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10),
      lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100, step = 10)
    )
  )

  # single value
  r <- sel(da, lat = 0)
  expect_equal(r@variable@dims, "lon")
  # lat=0 is index 2 (offset=-10, step=10, so 0 = -10 + 1*10 -> idx 2)
  expect_equal(as.vector(as.array(r)), c(2, 5, 8, 11))

  # range
  r2 <- sel(da, lat = c(-10, 0))
  expect_equal(r2@variable@dims, c("lat", "lon"))
  expect_equal(unname(shape(r2)), c(2L, 4L))
})

test_that("sel range on ImplicitCoord is correct", {
  v <- Variable(
    dims = c("lat", "lon"),
    data = matrix(seq_len(180 * 360), 180, 360)
  )
  lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
  lon <- ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)
  da <- DataArray(variable = v, coords = list(lat = lat, lon = lon))

  # select tropics
  tropics <- sel(da, lat = c(-23.5, 23.5))
  # should be ~47 latitude bands
  expect_true(shape(tropics)["lat"] >= 46L)
  expect_true(shape(tropics)["lat"] <= 48L)
})

test_that("isel on Dataset applies per-variable", {
  ds <- Dataset(
    data_vars = list(
      temp = Variable(dims = c("time", "lat"), data = matrix(1:12, 3, 4)),
      precip = Variable(dims = c("lat"), data = 1:4)
    ),
    coords = list(
      time = ExplicitCoord(dimension = "time", values = c("Jan", "Feb", "Mar")),
      lat = ImplicitCoord(dimension = "lat", n = 4L, offset = -30, step = 20)
    )
  )

  r <- isel(ds, lat = 1:2)
  expect_equal(unname(shape(r@data_vars$temp)), c(3L, 2L))
  expect_equal(unname(shape(r@data_vars$precip)), 2L)
  expect_equal(coord_length(r@coords$lat), 2L)
})
