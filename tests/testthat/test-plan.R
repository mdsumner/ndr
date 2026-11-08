## nd_plan(): the cost of reading lazy data, before reading it

## v (x = 20, y = 12, time = 6), chunks (5, 4, 2): 4 x 3 x 3 = 36 chunks
make_plan_store <- function(dir) {
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  writeLines('{"zarr_format": 2}', file.path(dir, ".zgroup"))
  writeLines("{}", file.path(dir, ".zattrs"))
  v <- array(as.double(seq_len(20 * 12 * 6)), c(20, 12, 6))
  write_v2(file.path(dir, "v"), v, c(5, 4, 2), c("x", "y", "time"))
  write_v2(file.path(dir, "x"), as.double(1:20), 20, "x")
  write_v2(file.path(dir, "y"), as.double(1:12), 12, "y")
  write_v2(file.path(dir, "time"), as.double(1:6), 6, "time")
  dir
}

test_that("zaro engine: chunks, requests and transports of a selection", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("altarr")
  ds <- open_dataset(make_plan_store(file.path(tempdir(), "plan-z.zarr")),
                     engine = "zaro")
  p <- nd_plan(ds$v)
  expect_s3_class(p, "nd_plan")
  expect_identical(p$engine, "zaro")
  expect_equal(p$chunks, 36)
  expect_equal(p$chunks_total, 36)
  expect_equal(p$values, 20 * 12 * 6)
  expect_equal(p$requests, 36)
  expect_equal(p$unknown_size, 36)                   # native: whole objects
  expect_identical(names(p$transport), "local")
  expect_s3_class(p$refs, "zaro_plan")

  ## x 3:8 -> chunks 0, 1; y all -> 3; time 2 -> chunk 0
  s <- ds$v |> isel(x = 3:8, time = 2)
  p <- nd_plan(s)
  expect_equal(p$chunks, 6)
  expect_equal(unname(p$selection), c(6L, 12L, 1L))
  expect_equal(p$values, 72)
  expect_equal(p$chunk_values, 10 * 12 * 2)
  expect_setequal(p$refs$key, paste0("v/0.", rep(0:2, each = 2), ".", 0:1))

  ## selections compose: two isel()s plan as one
  p2 <- nd_plan(ds$v |> isel(x = 3:8) |> isel(time = 2))
  expect_equal(p2$chunks, p$chunks)
  expect_setequal(p2$refs$key, p$refs$key)
  p3 <- nd_plan(s |> isel(y = 1:2, x = 4:6))       # positions within s
  expect_equal(unname(p3$selection), c(3L, 2L, 1L))
  expect_equal(p3$chunks, 1)          # source x 6:8 -> chunk 1; y 1:2 -> chunk 0

  ## sel() goes through isel()
  p4 <- nd_plan(ds$v |> sel(x = c(3, 8), time = 2))
  expect_equal(p4$chunks, 6)

  out <- capture.output(print(p))
  expect_match(out[1], "nd_plan.*'v'.*zaro engine")
  expect_true(any(grepl("6 request", out)))
})

test_that("max_chunks: counts only above the limit", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("altarr")
  ds <- open_dataset(make_plan_store(file.path(tempdir(), "plan-max.zarr")),
                     engine = "zaro")
  p <- nd_plan(ds$v, max_chunks = 10)
  expect_equal(p$chunks, 36)
  expect_null(p$requests)
  expect_true(any(grepl("not planned", capture.output(print(p)))))
})

test_that("GDAL engine: chunks counted, requests not estimated", {
  skip_if_not_installed("GDAL7")
  skip_if_not_installed("altarr")
  ds <- open_dataset(make_plan_store(file.path(tempdir(), "plan-g.zarr")),
                     engine = "gdal")
  p <- nd_plan(ds$v |> isel(x = 3:8, time = 2))
  expect_identical(p$engine, "gdal")
  expect_equal(p$chunks, 6)
  expect_null(p$requests)
  expect_true(any(grepl("not estimated", capture.output(print(p)))))
})

test_that("in-memory data has nothing to read; Dataset gives a table", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("altarr")
  ds <- open_dataset(make_plan_store(file.path(tempdir(), "plan-ds.zarr")),
                     engine = "zaro")
  m <- nd_plan(collect(ds$v |> isel(time = 1)))
  expect_identical(m$engine, "memory")
  expect_true(any(grepl("nothing", capture.output(print(m)))))

  v <- Variable(dims = c("a", "b"), data = matrix(1:6, 2))
  expect_identical(nd_plan(v)$engine, "memory")

  tab <- nd_plan(ds)
  expect_s3_class(tab, "nd_plan_dataset")
  expect_identical(tab$var, "v")
  expect_equal(tab$chunks, 36)
  expect_true(is.na(tab$bytes))                     # sizes unknown
  expect_match(capture.output(print(tab))[1], "Dataset: 1 variable")
})

test_that("arithmetic leaves no stale source", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("altarr")
  ds <- open_dataset(make_plan_store(file.path(tempdir(), "plan-ops.zarr")),
                     engine = "zaro")
  r <- (ds$v |> isel(time = 1)) + 1
  expect_identical(nd_plan(r)$engine, "memory")
})
