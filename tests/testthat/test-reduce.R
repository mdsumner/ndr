test_that("nd_mean reduces along named dim", {
  v <- Variable(dims = c("time", "lat", "lon"), data = array(1:24, c(2, 3, 4)))

  # reduce time
  r <- nd_mean(v, "time")
  expect_equal(r@dims, c("lat", "lon"))
  expect_equal(unname(shape(r)), c(3L, 4L))
  # mean of c(1,2) = 1.5, mean of c(3,4) = 3.5, etc.
  expect_equal(as.array(r)[1, 1], 1.5)

  # reduce lat and lon
  r2 <- nd_mean(v, c("lat", "lon"))
  expect_equal(r2@dims, "time")
  expect_equal(unname(shape(r2)), 2L)
})

test_that("nd_sum works", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  r <- nd_sum(v, "x")
  expect_equal(r@dims, "y")
  expect_equal(as.vector(as.array(r)), c(3, 7, 11))
})

test_that("nd_mean to scalar works", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  r <- nd_mean(v, c("x", "y"))
  expect_equal(ndim(r), 0L)
  expect_equal(as.array(r)[1], mean(1:6))
})

test_that("nd_mean on DataArray drops reduced coords", {
  v <- Variable(dims = c("lat", "lon"), data = matrix(1:12, 3, 4))
  da <- DataArray(
    variable = v,
    coords = list(
      lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10),
      lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100, step = 10)
    ),
    name = "temp"
  )

  r <- nd_mean(da, "lat")
  expect_equal(r@variable@dims, "lon")
  expect_equal(names(r@coords), "lon")
  expect_equal(r@name, "temp")
})

test_that("reduction with na.rm works", {
  dat <- c(1, NA, 3, 4, NA, 6)
  v <- Variable(dims = c("x", "y"), data = matrix(dat, 2, 3))

  r <- nd_mean(v, "x", na.rm = TRUE)
  expect_false(any(is.na(as.array(r))))

  r2 <- nd_mean(v, "x", na.rm = FALSE)
  expect_true(any(is.na(as.array(r2))))
})

test_that("reduction on unknown dim errors", {
  v <- Variable(dims = c("x", "y"), data = matrix(1, 2, 3))
  expect_error(nd_mean(v, "z"), "not found")
})
