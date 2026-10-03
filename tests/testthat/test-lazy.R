## Tests for lazy (altarr) data in Variables

# a lazy copy of an in-memory array, fetched chunk by chunk
lazy_copy <- function(ref, chunk) {
  d <- dim(ref)
  chunk <- rep_len(as.integer(chunk), length(d))
  force(ref)
  altarr::altarr(d, chunk, function(chunks) {
    lapply(seq_len(nrow(chunks)), function(r) {
      s <- chunks[r, ] * chunk + 1L
      e <- pmin(s + chunk - 1L, d)
      as.vector(do.call(`[`, c(list(ref), Map(seq.int, s, e), list(drop = FALSE))))
    })
  }, type = typeof(ref))
}

stat <- function(x, nm) unname(altarr::altarr_stats(x)[nm])

ref3 <- array(as.double(seq_len(9 * 7 * 5)), c(9L, 7L, 5L))
ref3[c(3, 50, 200)] <- NA
dims3 <- c("lon", "lat", "time")

lazy_var <- function(ref = ref3, chunk = c(4, 3, 2)) {
  Variable(dims = dims3, data = lazy_copy(ref, chunk), attrs = list(units = "K"))
}
eager_var <- function(ref = ref3) Variable(dims = dims3, data = ref, attrs = list(units = "K"))


# --- construction and metadata ---

test_that("Variable holds altarr data without reading it", {
  skip_if_not_installed("altarr")
  v <- lazy_var()
  expect_true(ndr:::is_lazy(v@data))
  expect_equal(shape(v), c(lon = 9L, lat = 7L, time = 5L))
  out <- capture.output(print(v))
  expect_true(any(grepl("(lazy)", out, fixed = TRUE)))
  da <- DataArray(variable = v, name = "x")
  out <- capture.output(print(da))
  expect_true(any(grepl("lazy", out)))
  expect_equal(stat(v@data, "fetch_calls"), 0)
})

test_that("in-memory data is not lazy", {
  expect_false(ndr:::is_lazy(ref3))
})


# --- isel / sel ---

test_that("isel on lazy data is lazy and matches eager isel", {
  skip_if_not_installed("altarr")
  old <- options(altarr.max_materialize = 0)
  on.exit(options(old))
  v <- lazy_var()
  cases <- list(
    list(lat = 2:5),
    list(time = 3L),
    list(lon = c(9L, 1L, 1L), time = 2:4),
    list(lat = -1L * 2:3),
    list(lon = c(TRUE, FALSE), lat = 7L),
    list(lon = 4L, lat = 6L, time = 1L)
  )
  for (cs in cases) {
    lz <- do.call(isel, c(list(v), cs))
    ez <- do.call(isel, c(list(eager_var()), cs))
    expect_equal(lz@dims, ez@dims)
    if (length(lz@dims)) {
      expect_true(ndr:::is_lazy(lz@data))
      expect_identical(as.array(lz), ez@data)
    } else {
      expect_identical(lz@data, ez@data)
    }
  }
  expect_equal(stat(v@data, "elt"), 0)
})

test_that("selecting reads nothing; reading a selection touches only its chunks", {
  skip_if_not_installed("altarr")
  v <- lazy_var()
  s <- isel(v, lon = 1:4, lat = 1:3)
  expect_equal(stat(v@data, "fetch_calls"), 0)
  vals <- collect(s)@data
  expect_identical(vals, ref3[1:4, 1:3, , drop = FALSE])
  # one chunk column along time: 3 chunks, one planned read
  expect_equal(stat(v@data, "chunks_fetched"), 3)
  expect_equal(stat(v@data, "fetch_calls"), 1)
  expect_equal(stat(v@data, "elt"), 0)
})

test_that("chained isel composes", {
  skip_if_not_installed("altarr")
  r1 <- lazy_var() |> isel(lat = 2:6) |> isel(lat = 2:3, time = 5L)
  r2 <- eager_var() |> isel(lat = 3:4, time = 5L)
  expect_identical(as.array(r1), r2@data)
})

test_that("sel on a lazy DataArray is lazy and slices coords", {
  skip_if_not_installed("altarr")
  da <- DataArray(
    variable = lazy_var(),
    coords = list(
      lon = ImplicitCoord(dimension = "lon", n = 9L, offset = 100, step = 1),
      lat = ImplicitCoord(dimension = "lat", n = 7L, offset = -30, step = 10),
      time = ExplicitCoord(dimension = "time", values = as.Date("2020-01-01") + 0:4)
    ),
    name = "x"
  )
  ez <- DataArray(variable = eager_var(), coords = da@coords, name = "x")
  lz <- sel(da, lat = c(-20, 10), time = as.Date("2020-01-03"))
  ee <- sel(ez, lat = c(-20, 10), time = as.Date("2020-01-03"))
  expect_true(ndr:::is_lazy(lz@variable@data))
  expect_equal(names(lz@coords), names(ee@coords))
  expect_identical(as.array(lz), ee@variable@data)
})

test_that("an empty selection gives an empty array", {
  skip_if_not_installed("altarr")
  r <- isel(lazy_var(), lat = integer(0))
  expect_equal(shape(r), c(lon = 9L, lat = 0L, time = 5L))
})


# --- reductions ---

test_that("lazy reductions match eager ones", {
  skip_if_not_installed("altarr")
  old <- options(altarr.max_materialize = 0, ndr.block_values = 40)
  on.exit(options(old))
  over <- list("time", "lon", c("lon", "time"), c("lat", "time"), dims3)
  fns <- list(nd_mean, nd_sum, nd_min, nd_max)
  for (fn in fns) for (d in over) for (na.rm in c(FALSE, TRUE)) {
    lz <- fn(lazy_var(), d, na.rm = na.rm)
    ez <- fn(eager_var(), d, na.rm = na.rm)
    expect_equal(lz@dims, ez@dims)
    expect_equal(lz@data, ez@data)
  }
})

test_that("lazy reductions stream chunk-aligned blocks, never element by element", {
  skip_if_not_installed("altarr")
  old <- options(altarr.max_materialize = 0, ndr.block_values = 4 * 3 * 2 * 3)
  on.exit(options(old))
  v <- lazy_var()
  r <- nd_mean(v, "time", na.rm = TRUE)
  expect_equal(stat(v@data, "elt"), 0)
  # 3 x 3 x 3 chunks; blocks span the whole time axis (3 chunks): 9 reads
  expect_equal(stat(v@data, "chunks_fetched"), 27)
  expect_equal(stat(v@data, "fetch_calls"), 9)
})

test_that("reductions on a lazy selection read only the selection", {
  skip_if_not_installed("altarr")
  v <- lazy_var()
  r <- v |> isel(lon = 1:4, lat = 1:3) |> nd_max("time", na.rm = TRUE)
  expect_equal(r@data, apply(ref3[1:4, 1:3, ], 1:2, max, na.rm = TRUE))
  expect_equal(stat(v@data, "chunks_fetched"), 3)
})

test_that("integer reductions keep base R's types", {
  skip_if_not_installed("altarr")
  iref <- array(seq_len(6 * 4), c(6L, 4L))
  iref[2] <- NA
  lz <- Variable(dims = c("x", "y"), data = lazy_copy(iref, c(4, 3)))
  ez <- Variable(dims = c("x", "y"), data = iref)
  for (fn in list(nd_sum, nd_min, nd_max, nd_mean)) {
    for (na.rm in c(FALSE, TRUE)) {
      expect_identical(fn(lz, "y", na.rm = na.rm)@data, fn(ez, "y", na.rm = na.rm)@data)
      expect_identical(fn(lz, c("x", "y"), na.rm = na.rm)@data,
                       fn(ez, c("x", "y"), na.rm = na.rm)@data)
    }
  }
})

test_that("min/max of all-NA cells warn and give Inf as base R does", {
  skip_if_not_installed("altarr")
  nref <- array(c(NA, NA, 1, 2), c(2L, 2L))
  lz <- Variable(dims = c("x", "y"), data = lazy_copy(nref, 1))
  expect_warning(r <- nd_min(lz, "x", na.rm = TRUE), "no non-missing")
  expect_equal(as.vector(r@data), c(Inf, 1))
})

test_that("reduction blocks are whole chunks grown towards the target", {
  expect_equal(ndr:::reduce_block_shape(c(10L, 10L, 10L), c(3L, 3L, 2L), 3L, 60),
               c(3L, 3L, 6L))
  expect_equal(ndr:::reduce_block_shape(c(10L, 10L, 10L), c(3L, 3L, 2L), 3L, 1e6),
               c(10L, 10L, 10L))
})


# --- arithmetic, coercion, collect ---

test_that("arithmetic on lazy data reads it and matches eager", {
  skip_if_not_installed("altarr")
  old <- options(altarr.max_materialize = 0)
  on.exit(options(old))
  lz <- lazy_var() + 273.15
  ez <- eager_var() + 273.15
  expect_false(ndr:::is_lazy(lz@data))
  expect_identical(lz@data, ez@data)
  mask <- Variable(dims = c("lat", "lon"), data = matrix(1:63 %% 2, 7, 9))
  expect_identical((lazy_var() * mask)@data, (eager_var() * mask)@data)
})

test_that("collect() reads lazy data into memory", {
  skip_if_not_installed("altarr")
  v <- lazy_var()
  cv <- collect(v)
  expect_false(ndr:::is_lazy(cv@data))
  expect_identical(cv@data, ref3)
  expect_equal(cv@attrs, v@attrs)
  da <- DataArray(variable = v, name = "x")
  expect_identical(collect(da)@variable@data, ref3)
  ds <- Dataset(data_vars = list(x = v))
  expect_identical(collect(ds)@data_vars$x@data, ref3)
})

test_that("collect() on in-memory objects is identity", {
  v <- Variable(dims = c("x", "y"), data = matrix(1:6, 2, 3))
  da <- DataArray(variable = v, name = "test")
  expect_identical(collect(v), v)
  expect_identical(collect(da), da)
})

test_that("as.array and as.data.frame read lazy data", {
  skip_if_not_installed("altarr")
  old <- options(altarr.max_materialize = 0)
  on.exit(options(old))
  da <- DataArray(variable = isel(lazy_var(), time = 1L), name = "x")
  expect_identical(as.array(da), ref3[, , 1])
  df <- as.data.frame(da)
  expect_equal(df$x, as.vector(ref3[, , 1]))
})

test_that("a lazy selection saves as a recipe and reads after reload", {
  skip_if_not_installed("altarr")
  s <- isel(lazy_var(), lat = 2:4)
  f <- tempfile(fileext = ".rds")
  saveRDS(s, f)
  s2 <- readRDS(f)
  expect_true(ndr:::is_lazy(s2@data))
  expect_identical(as.array(s2), ref3[, 2:4, ])
})

# --- GDAL backend (needs a local file) ---

oisst_dsn <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"

skip_oisst <- function() {
  skip_if_not_installed("altarr")
  skip_if_not(file.exists(oisst_dsn), "OISST test data not available")
}

test_that("ds$var returns a DataArray with lazy data", {
  skip_oisst()
  ds <- open_dataset(oisst_dsn)
  sst <- ds$sst
  expect_true(S7_inherits(sst, DataArray))
  expect_true(ndr:::is_lazy(sst@variable@data))
  expect_equal(sst@name, "sst")
  expect_equal(sst@variable@dims, c("lon", "lat", "time"))
  expect_equal(unname(shape(sst)), c(360L, 180L, 494L))
  expect_true(all(c("lat", "lon", "time") %in% names(sst@coords)))
})

test_that("lazy sel + collect equals eager sel", {
  skip_oisst()
  ds <- open_dataset(oisst_dsn)
  lazy_r <- ds$sst |> sel(lat = c(-60, -30)) |> isel(time = 1L) |> collect()
  eager_r <- ds$sst |> isel(time = 1L) |> collect() |> sel(lat = c(-60, -30))
  expect_identical(lazy_r@variable@data, eager_r@variable@data)
  expect_equal(lazy_r@variable@dims, c("lon", "lat"))
  expect_false("time" %in% names(lazy_r@coords))
})

test_that("reduction on GDAL-backed data streams", {
  skip_oisst()
  ds <- open_dataset(oisst_dsn)
  sub <- ds$sst |> isel(lon = 1:5, lat = 1:5, time = 1:10)
  r <- nd_mean(sub, "time", na.rm = TRUE)
  e <- nd_mean(collect(sub), "time", na.rm = TRUE)
  expect_equal(r@variable@dims, c("lon", "lat"))
  expect_equal(r@variable@data, e@variable@data)
})


# --- GDAL backend on a small chunked NetCDF fixture ---
# fixtures/chunked.nc is built from fixtures/chunked.cdl with
# `ncgen -k nc4 -o chunked.nc chunked.cdl`: temp(time, lat, lon) holds
# t*1000 + y*10 + x (0-based) with two missing values, chunked 2 x 3 x 4;
# cnt is a short array chunked 4 x 5 x 3; packed is a short array holding
# t*100 + y*10 + x with scale_factor 0.5, add_offset 10 and one missing value.

skip_gdal_mdim <- function() {
  skip_if_not_installed("altarr")
  skip_if_not_installed("GDAL7")
}

chunked_nc <- function() test_path("fixtures", "chunked.nc")

chunked_ref <- function() {
  ref <- outer(outer(0:6, 10 * (0:4), `+`), 1000 * (0:5), `+`)
  ref[4, 3, 2] <- NA
  ref[1, 1, 5] <- NA
  ref
}

test_that("GDAL-backed variables are lazy and chunked like the file", {
  skip_gdal_mdim()
  ds <- open_dataset(chunked_nc())
  temp <- ds$temp
  a <- temp@variable@data
  expect_true(ndr:::is_lazy(a))
  expect_equal(temp@variable@dims, c("lon", "lat", "time"))
  # NetCDF chunks 2 x 3 x 4 (time, lat, lon) are 4 x 3 x 2 in R order
  expect_equal(ndr:::lazy_chunk(a), c(4L, 3L, 2L))
  expect_equal(stat(a, "fetch_calls"), 0)
  expect_equal(collect(temp)@variable@data, chunked_ref())
})

test_that("GDAL-backed sel, reductions and integer sources", {
  skip_gdal_mdim()
  ds <- open_dataset(chunked_nc())
  ref <- chunked_ref()
  s <- ds$temp |> sel(lat = c(-30, 0)) |> isel(time = 2:4)
  expect_true(ndr:::is_lazy(s@variable@data))
  expect_equal(collect(s)@variable@data, ref[, 2:5, 2:4])
  m <- nd_mean(s, "time", na.rm = TRUE)
  expect_equal(m@variable@data, apply(ref[, 2:5, 2:4], 1:2, mean, na.rm = TRUE))

  # GDAL7's as_altarr() reads every type as double for now
  cnt <- ds$cnt
  expect_equal(ndr:::lazy_chunk(cnt@variable@data), c(3L, 5L, 4L))
  iref <- outer(outer(0:6, 10 * (0:4), `+`), 1000 * (0:5), `+`)
  expect_equal(collect(cnt)@variable@data, iref)
  expect_equal(as.vector(nd_max(cnt, c("lon", "lat"))@variable@data),
               1000 * (0:5) + 46)
})

test_that("GDAL-backed packed arrays are unpacked lazily", {
  skip_gdal_mdim()
  ds <- open_dataset(chunked_nc())
  p <- ds$packed
  expect_true(ndr:::is_lazy(p@variable@data))
  raw <- outer(outer(0:6, 10 * (0:4), `+`), 100 * (0:5), `+`)
  raw[2, 2, 3] <- NA
  expect_equal(collect(p)@variable@data, raw * 0.5 + 10)
  expect_equal(collect(isel(p, time = 3L))@variable@data, raw[, , 3] * 0.5 + 10)
})

test_that("a GDAL-backed selection saves as a recipe", {
  skip_gdal_mdim()
  ds <- open_dataset(chunked_nc())
  s <- ds$temp |> isel(lat = 2:3)
  f <- tempfile(fileext = ".rds")
  saveRDS(s, f)
  expect_equal(collect(readRDS(f))@variable@data, chunked_ref()[, 2:3, ])
})
