test_that("ImplicitCoord values are computed correctly", {
  lat <- ImplicitCoord(dimension = "lat", n = 5L, offset = -2.0, step = 1.0)
  expect_equal(coord_values(lat), c(-2, -1, 0, 1, 2))
  expect_equal(coord_length(lat), 5L)
  expect_equal(coord_dim(lat), "lat")
})

test_that("ImplicitCoord lookup is O(1) inverse", {
  lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
  # exact hit

  expect_equal(coord_lookup(lat, -89.5), 1L)
  expect_equal(coord_lookup(lat, 89.5), 180L)
  expect_equal(coord_lookup(lat, 0.5), 91L)
  # nearest
  expect_equal(coord_lookup(lat, 0.3), 91L)
})

test_that("ImplicitCoord slicing preserves implicitness for regular subsets", {
  lon <- ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)
  sliced <- coord_slice(lon, 10:20)
  expect_s3_class(sliced, "ImplicitCoord")
  expect_equal(sliced@n, 11L)
  expect_equal(sliced@offset, 9.5)
  expect_equal(sliced@step, 1.0)
})

test_that("ImplicitCoord slicing materializes for irregular subsets", {
  lon <- ImplicitCoord(dimension = "lon", n = 10L, offset = 0.5, step = 1.0)
  sliced <- coord_slice(lon, c(1, 3, 7))
  expect_s3_class(sliced, "ExplicitCoord")
  expect_equal(sliced@values, c(0.5, 2.5, 6.5))
})

test_that("ExplicitCoord works with dates", {
  dates <- as.Date("2020-01-01") + 0:9
  tc <- ExplicitCoord(dimension = "time", values = dates)
  expect_equal(coord_length(tc), 10L)
  expect_equal(coord_values(tc), dates)
})

test_that("ExplicitCoord lookup finds nearest for numeric", {
  ec <- ExplicitCoord(dimension = "x", values = c(1, 2, 4, 8, 16))
  expect_equal(coord_lookup(ec, 7), 4L)   # nearest to 8
  expect_equal(coord_lookup(ec, 8), 4L)   # exact
  expect_equal(coord_lookup(ec, 1), 1L)   # first
  expect_equal(coord_lookup(ec, 20), 5L)  # beyond range -> last
})
