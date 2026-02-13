test_that("broadcasting shape computation works", {
  a <- Variable(dims = c("time", "lat", "lon"), data = array(0, c(10, 3, 4)))
  b <- Variable(dims = c("lat", "lon"), data = matrix(0, 3, 4))

  s <- ndr:::broadcast_shape(a, b)
  expect_equal(s, c(time = 10L, lat = 3L, lon = 4L))
})

test_that("broadcasting detects dimension size mismatch", {
  a <- Variable(dims = c("x", "y"), data = matrix(0, 3, 4))
  b <- Variable(dims = c("x", "y"), data = matrix(0, 5, 4))

  expect_error(ndr:::broadcast_shape(a, b), "cannot broadcast")
})

test_that("Variable arithmetic broadcasts by name", {
  # temperature (time, lat, lon) * mask (lat, lon)
  temp <- Variable(
    dims = c("time", "lat", "lon"),
    data = array(1, c(2, 3, 4))
  )
  mask <- Variable(
    dims = c("lat", "lon"),
    data = matrix(c(1,0,1, 1,0,1, 1,0,1, 1,0,1), 3, 4)
  )

  result <- temp * mask
  expect_equal(result@dims, c("time", "lat", "lon"))
  expect_equal(unname(shape(result)), c(2L, 3L, 4L))

  # check the masking worked: row 2 (lat index 2) should be 0
  arr <- as.array(result)
  expect_true(all(arr[, 2, ] == 0))
  expect_true(all(arr[, 1, ] == 1))
  expect_true(all(arr[, 3, ] == 1))
})

test_that("Variable arithmetic with scalar", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  result <- v + 10
  expect_equal(result@dims, c("x", "y"))
  expect_equal(as.array(result), matrix(11:16, 2, 3))
})

test_that("broadcasting handles disjoint dims (outer product)", {
  row <- Variable(dims = "x", data = 1:3)
  col <- Variable(dims = "y", data = 1:4)
  result <- row * col
  expect_equal(result@dims, c("x", "y"))
  expect_equal(unname(shape(result)), c(3L, 4L))
  # should be outer product
  expect_equal(as.array(result), outer(1:3, 1:4))
})

test_that("broadcasting handles reversed dim order", {
  a <- Variable(dims = c("lat", "lon"), data = matrix(1:6, 2, 3))
  b <- Variable(dims = c("lon", "lat"), data = matrix(10:15, 3, 2))
  result <- a + b
  expect_equal(result@dims, c("lat", "lon"))
  # b should be transposed to match a's dim order
  expected <- matrix(1:6, 2, 3) + t(matrix(10:15, 3, 2))
  expect_equal(as.array(result), expected)
})

test_that("DataArray arithmetic preserves coordinates", {
  v1 <- Variable(dims = c("lat", "lon"), data = matrix(1, 3, 4))
  lat <- ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10)
  lon <- ImplicitCoord(dimension = "lon", n = 4L, offset = 100, step = 10)

  da <- DataArray(variable = v1, coords = list(lat = lat, lon = lon), name = "temp")
  result <- da * 2

  expect_s3_class(result, "DataArray")
  expect_equal(length(result@coords), 2L)
  expect_equal(coord_dim(result@coords$lat), "lat")
})

test_that("comparison ops work on Variables", {
  v <- Variable(dims = "x", data = c(1, 5, 10, 15, 20))
  result <- v > 10
  expect_equal(as.array(result), array(c(FALSE, FALSE, FALSE, TRUE, TRUE)))
})
