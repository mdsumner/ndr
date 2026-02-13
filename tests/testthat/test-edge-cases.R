# --- Variable edge cases ---

test_that("Variable from bare vector (no dim attribute)", {
  v <- Variable(dims = "x", data = c(1, 2, 3))
  expect_equal(shape(v), c(x = 3L))
  expect_equal(ndim(v), 1L)
  # var_data should give it a dim
  d <- var_data(v)
  expect_equal(dim(d), 3L)
})

test_that("Variable from matrix", {
  v <- Variable(dims = c("r", "c"), data = matrix(1:6, 2, 3))
  expect_equal(shape(v), c(r = 2L, c = 3L))
})

test_that("Variable with integer data", {
  v <- Variable(dims = "x", data = array(1:5, dim = 5))
  expect_equal(typeof(v@data), "integer")
  expect_equal(shape(v), c(x = 5L))
})

test_that("Variable scalar round-trips", {
  s <- Variable(dims = character(), data = array(42))
  expect_equal(ndim(s), 0L)
  expect_equal(length(s), 1L)
  expect_null(dim(s))
  d <- var_data(s)
  expect_equal(d[1], 42)
})

test_that("Variable rejects mismatched dims", {
  expect_error(Variable(dims = c("x", "y"), data = 1:3))
  expect_error(Variable(dims = character(), data = 1:3))
  expect_error(Variable(dims = c("x", "x"), data = matrix(1:4, 2, 2)))
})


# --- Coordinate edge cases ---

test_that("ImplicitCoord n=1 works", {
  c1 <- ImplicitCoord(dimension = "x", n = 1L, offset = 42.0, step = 0.0)
  expect_equal(coord_values(c1), 42.0)
  expect_equal(coord_length(c1), 1L)
  # lookup should always return 1L
  expect_equal(coord_lookup(c1, 42), 1L)
  expect_equal(coord_lookup(c1, 999), 1L)
})

test_that("ImplicitCoord n=0 is allowed and empty", {
  c0 <- ImplicitCoord(dimension = "x", n = 0L, offset = 0.0, step = 1.0)
  expect_equal(coord_length(c0), 0L)
  expect_equal(length(coord_values(c0)), 0L)
})

test_that("ExplicitCoord with single value", {
  ec <- ExplicitCoord(dimension = "z", values = 100)
  expect_equal(coord_length(ec), 1L)
  expect_equal(coord_lookup(ec, 100), 1L)
})

test_that("ExplicitCoord with Date values", {
  dates <- as.Date("2020-01-01") + c(0, 10, 20, 30)
  ec <- ExplicitCoord(dimension = "time", values = dates)
  expect_equal(coord_length(ec), 4L)
  # exact match
  expect_equal(coord_lookup(ec, as.Date("2020-01-11")), 2L)
  # no match returns NA (Date uses match())
  expect_true(is.na(coord_lookup(ec, as.Date("2020-01-05"))))
})

test_that("ExplicitCoord with character values", {
  ec <- ExplicitCoord(dimension = "band", values = c("red", "green", "blue"))
  expect_equal(coord_lookup(ec, "green"), 2L)
  expect_true(is.na(coord_lookup(ec, "alpha")))
})

test_that("ExplicitCoord lookup on descending numeric finds nearest", {
  ec <- ExplicitCoord(dimension = "depth", values = c(100, 50, 20, 5))
  # should find nearest even though unsorted
  expect_equal(coord_lookup(ec, 50), 2L)
  expect_equal(coord_lookup(ec, 48), 2L)
  expect_equal(coord_lookup(ec, 6), 4L)
})

test_that("ImplicitCoord lookup clamps to valid range", {
  lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
  # way below range
  expect_equal(coord_lookup(lat, -1000), 1L)
  # way above range
  expect_equal(coord_lookup(lat, 1000), 180L)
})

test_that("coord_slice preserves ExplicitCoord type for Date", {
  dates <- as.Date("2020-01-01") + 0:9
  ec <- ExplicitCoord(dimension = "time", values = dates)
  sliced <- coord_slice(ec, 3:5)
  expect_equal(coord_values(sliced), dates[3:5])
  expect_true(inherits(coord_values(sliced), "Date"))
})


# --- Broadcasting edge cases ---

test_that("scalar * scalar", {
  a <- Variable(dims = character(), data = array(3))
  b <- Variable(dims = character(), data = array(7))
  r <- a * b
  expect_equal(ndim(r), 0L)
  expect_equal(var_data(r)[1], 21)
})

test_that("scalar + 1D", {
  v <- Variable(dims = "x", data = c(1, 2, 3))
  r <- v + 10
  expect_equal(r@dims, "x")
  expect_equal(as.vector(var_data(r)), c(11, 12, 13))
})

test_that("NAs propagate through broadcasting", {
  a <- Variable(dims = c("x", "y"), data = matrix(c(1, NA, 3, 4), 2, 2))
  b <- Variable(dims = "x", data = c(10, 20))
  r <- a + b
  arr <- var_data(r)
  expect_true(is.na(arr[2, 1]))
  expect_equal(arr[1, 1], 11)
  expect_equal(arr[1, 2], 13)
})

test_that("broadcast errors on mismatched shared dim sizes", {
  a <- Variable(dims = c("x", "y"), data = matrix(0, 3, 4))
  b <- Variable(dims = c("x", "y"), data = matrix(0, 5, 4))
  expect_error(a + b, "cannot broadcast")
})

test_that("1D * 1D same dim", {
  a <- Variable(dims = "x", data = c(1, 2, 3))
  b <- Variable(dims = "x", data = c(10, 20, 30))
  r <- a * b
  expect_equal(r@dims, "x")
  expect_equal(as.vector(var_data(r)), c(10, 40, 90))
})

test_that("3 + scalar Variable works both ways", {
  v <- Variable(dims = "x", data = c(1, 2, 3))
  r1 <- v + 5
  r2 <- 5 + v
  expect_equal(as.vector(var_data(r1)), c(6, 7, 8))
  expect_equal(as.vector(var_data(r2)), c(6, 7, 8))
})


# --- Indexing edge cases ---

test_that("isel with integer(0) gives 0-length dim", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:12, 3, 4))
  # This should produce a valid Variable with x=0
  r <- isel(v, x = integer(0))
  expect_equal(shape(r)["x"], c(x = 0L))
})

test_that("isel preserves type (integer data stays integer)", {
  v <- Variable(dims = "x", data = array(1:10, dim = 10L))
  r <- isel(v, x = 1:3)
  expect_equal(typeof(var_data(r)), "integer")
})

test_that("isel scalar result from multi-dim", {
  v <- Variable(dims = c("x", "y", "z"), data = array(1:24, c(2, 3, 4)))
  r <- isel(v, x = 1, y = 2, z = 3)
  expect_equal(ndim(r), 0L)
  expect_equal(var_data(r)[1], v@data[1, 2, 3])
})

test_that("sel outside coordinate range clamps for ImplicitCoord", {
  v <- Variable(dims = "x", data = c(10, 20, 30))
  da <- DataArray(
    variable = v,
    coords = list(
      x = ImplicitCoord(dimension = "x", n = 3L, offset = 0.0, step = 1.0)
    )
  )
  # sel way beyond range should clamp to last element
  r <- sel(da, x = 999)
  expect_equal(ndim(r@variable), 0L)
  expect_equal(var_data(r@variable)[1], 30)
})

test_that("sel range with no matching values gives empty result", {
  v <- Variable(dims = "x", data = c(10, 20, 30))
  da <- DataArray(
    variable = v,
    coords = list(
      x = ImplicitCoord(dimension = "x", n = 3L, offset = 0.0, step = 1.0)
    )
  )
  # range selection where no coords fall in range
  r <- sel(da, x = c(100, 200))
  expect_equal(shape(r@variable)["x"], c(x = 0L))
})

test_that("sel on missing coord dimension errors", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  da <- DataArray(variable = v, coords = list())
  expect_error(sel(da, x = 1), "no coordinate found")
})

test_that("isel on nonexistent dim errors", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  expect_error(isel(v, z = 1), "not found")
})

test_that("sel with Date coordinates (range)", {
  dates <- as.Date("2020-01-01") + 0:9
  v <- Variable(dims = "time", data = 1:10 * 10)
  da <- DataArray(
    variable = v,
    coords = list(
      time = ExplicitCoord(dimension = "time", values = dates)
    )
  )
  # range: days 3-7
  r <- sel(da, time = as.Date(c("2020-01-03", "2020-01-07")))
  expect_equal(shape(r@variable)["time"], c(time = 5L))
  expect_equal(as.vector(var_data(r@variable)), c(30, 40, 50, 60, 70))
})


# --- Reduction edge cases ---

test_that("reduce 1D to scalar", {
  v <- Variable(dims = "x", data = c(1, 2, 3, 4, 5))
  r <- nd_sum(v, "x")
  expect_equal(ndim(r), 0L)
  expect_equal(var_data(r)[1], 15)
})

test_that("sequential reductions", {
  v <- Variable(dims = c("x", "y", "z"), data = array(1:24, c(2, 3, 4)))
  r1 <- nd_mean(v, "z")
  expect_equal(r1@dims, c("x", "y"))
  r2 <- nd_mean(r1, "x")
  expect_equal(r2@dims, "y")
})

test_that("reduce preserves attrs", {
  v <- Variable(dims = "x", data = 1:5, attrs = list(units = "m"))
  r <- nd_mean(v, "x")
  expect_equal(r@attrs$units, "m")
})

test_that("reduce on nonexistent dim errors", {
  v <- Variable(dims = "x", data = 1:5)
  expect_error(nd_mean(v, "z"), "not found")
})

test_that("reduce 2D keeping single dim", {
  # 2x3 matrix, reduce "x" keeping "y"
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  r <- nd_sum(v, "x")
  expect_equal(r@dims, "y")
  expect_equal(as.vector(var_data(r)), c(3, 7, 11))
})


# --- Dataset edge cases ---

test_that("Dataset $ and [[ extraction work", {
  ds <- Dataset(
    data_vars = list(
      a = Variable(dims = "x", data = 1:3),
      b = Variable(dims = "x", data = 4:6)
    ),
    coords = list(
      x = ExplicitCoord(dimension = "x", values = c(10, 20, 30))
    )
  )
  da <- ds$a
  expect_true(S7_inherits(da, DataArray))
  expect_equal(da@name, "a")

  da2 <- ds[["b"]]
  expect_true(S7_inherits(da2, DataArray))
  expect_equal(da2@name, "b")
})

test_that("Dataset rejects mismatched shared dim sizes", {
  expect_error(Dataset(
    data_vars = list(
      a = Variable(dims = "x", data = 1:3),
      b = Variable(dims = "x", data = 1:5)
    )
  ))
})

test_that("Dataset with variables of different dimensionality", {
  ds <- Dataset(
    data_vars = list(
      temp = Variable(dims = c("time", "lat"), data = matrix(0, 10, 3)),
      mask = Variable(dims = "lat", data = c(1, 0, 1))
    ),
    coords = list(
      lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -10.0, step = 10.0)
    )
  )
  # ds_dims should report both time and lat
  dims <- ds_dims(ds)
  expect_true("time" %in% names(dims))
  expect_true("lat" %in% names(dims))
  expect_equal(unname(dims["lat"]), 3L)
})

test_that("Dataset $ on nonexistent variable errors", {
  ds <- Dataset(data_vars = list(a = Variable(dims = "x", data = 1:3)))
  expect_error(ds$z, "no variable")
})

test_that("ds_dims does not mangle names", {
  ds <- Dataset(
    data_vars = list(
      a = Variable(dims = c("lat", "lon"), data = matrix(0, 3, 4))
    ),
    coords = list(
      lat = ImplicitCoord(dimension = "lat", n = 3L, offset = 0.0, step = 1.0),
      lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 0.0, step = 1.0)
    )
  )
  dims <- ds_dims(ds)
  expect_equal(names(dims), c("lat", "lon"))
  expect_equal(unname(dims), c(3L, 4L))
})


# --- as.data.frame edge cases ---

test_that("as.data.frame on 1D DataArray", {
  da <- DataArray(
    variable = Variable(dims = "x", data = c(10, 20, 30)),
    coords = list(x = ExplicitCoord(dimension = "x", values = c(1, 2, 3))),
    name = "val"
  )
  df <- as.data.frame(da)
  expect_equal(nrow(df), 3L)
  expect_equal(names(df), c("x", "val"))
  expect_equal(df$val, c(10, 20, 30))
})

test_that("as.data.frame with no coords uses integer indices", {
  da <- DataArray(
    variable = Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3)),
    coords = list(),
    name = "v"
  )
  df <- as.data.frame(da)
  expect_equal(nrow(df), 6L)
  expect_equal(df$x, rep(1:2, 3))
})

test_that("as.data.frame preserves Date coordinates", {
  dates <- as.Date("2020-01-01") + 0:2
  da <- DataArray(
    variable = Variable(dims = "time", data = c(1, 2, 3)),
    coords = list(time = ExplicitCoord(dimension = "time", values = dates)),
    name = "val"
  )
  df <- as.data.frame(da)
  expect_true(inherits(df$time, "Date"))
  expect_equal(df$time, dates)
})
