test_that("Variable construction works", {
  arr <- array(1:24, dim = c(2, 3, 4))
  v <- Variable(dims = c("x", "y", "z"), data = arr)
  expect_equal(v@dims, c("x", "y", "z"))
  expect_equal(shape(v), c(x = 2L, y = 3L, z = 4L))
  expect_equal(ndim(v), 3L)
  expect_equal(length(v), 24L)
})

test_that("Variable scalar works", {
  s <- Variable(dims = character(), data = array(42))
  expect_equal(ndim(s), 0L)
  expect_equal(length(s), 1L)
  expect_equal(shape(s), stats::setNames(integer(), character()))
})

test_that("Variable from 1D vector works", {
  v <- Variable(dims = "time", data = 1:10)
  expect_equal(shape(v), c(time = 10L))
})

test_that("Variable validates dims vs data", {
  expect_error(
    Variable(dims = c("x", "y"), data = array(1:6, c(2, 3, 1))),
    "dimensions"
  )
  expect_error(
    Variable(dims = c("x", "x"), data = matrix(1:4, 2, 2)),
    "unique"
  )
})

test_that("as.array works", {
  arr <- matrix(1:6, 2, 3)
  v <- Variable(dims = c("x", "y"), data = arr)
  expect_identical(as.array(v), arr)
})
