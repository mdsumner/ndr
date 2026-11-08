## open_dataset(group =) on a small local nested V2 store, both engines

test_that("split_zarr_group takes a group off after .zarr/", {
  expect_equal(split_zarr_group("https://h/p.zarr/a/b/"),
               list(dsn = "https://h/p.zarr", group = "a/b"))
  expect_equal(split_zarr_group("s3://bkt/P.ZARR/r10m"),
               list(dsn = "s3://bkt/P.ZARR", group = "r10m"))
  expect_equal(split_zarr_group("/data/x.zarr/")$group, "")
  expect_equal(split_zarr_group("/data/x.nc")$group, "")
  expect_equal(split_zarr_group('ZARR:"/vsicurl/https://x/y.zarr/g"')$group, "")
  expect_equal(split_zarr_group("/vsis3/b/y.zarr/g")$group, "")
  expect_equal(clean_group("/a/b/"), "a/b")
  expect_equal(clean_group(NULL), "")
  expect_error(clean_group(c("a", "b")), "single path")
})

## root: lon (unrelated); group meas/r10: x, y and band (x, y)
make_nested <- function(path) {
  unlink(path, recursive = TRUE)
  g <- file.path(path, "meas", "r10")
  dir.create(g, recursive = TRUE)
  for (d in c("", "meas", "meas/r10")) {
    writeLines('{"zarr_format": 2}', file.path(path, d, ".zgroup"))
  }
  writeLines('{"title": "root"}', file.path(path, ".zattrs"))
  writeLines('{"proj:code": "EPSG:32636"}', file.path(g, ".zattrs"))
  write_v2(file.path(path, "lon"), c(1, 2, 3), chunk = 3L, dims = "lon")
  write_v2(file.path(g, "x"), c(5, 15, 25, 35), chunk = 4L, dims = "x")
  write_v2(file.path(g, "y"), c(95, 85, 75), chunk = 3L, dims = "y")
  band <- array(as.double(1:12), c(4L, 3L))
  write_v2(file.path(g, "band"), band, chunk = c(2L, 2L), dims = c("x", "y"))
  files <- list.files(path, pattern = "^\\.z", recursive = TRUE, all.files = TRUE)
  entries <- vapply(files, function(f) {
    sprintf('"%s": %s', f, paste(readLines(file.path(path, f)), collapse = ""))
  }, character(1))
  writeLines(sprintf('{"zarr_consolidated_format": 1, "metadata": {%s}}',
                     paste(entries, collapse = ", ")),
             file.path(path, ".zmetadata"))
  list(path = path, band = band)
}

check_group_ds <- function(ds, band) {
  expect_setequal(names(ds@coords), c("x", "y"))
  expect_true("band" %in% names(ds))
  expect_equal(ds@attrs[["proj:code"]], "EPSG:32636")
  b <- ds$band
  expect_equal(b@variable@dims, c("x", "y"))
  expect_equal(as.array(collect(b)), band, ignore_attr = TRUE)
  sub <- b |> sel(x = c(10, 30)) |> collect()
  expect_equal(as.array(sub), band[2:3, ], ignore_attr = TRUE)
}

test_that("zaro engine opens a group, by argument or in the path", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("altarr")
  skip_if_not("group" %in% names(formals(zaro::zaro_dataset)),
              "zaro without zaro_dataset(group =)")
  s <- make_nested(file.path(tempdir(), "grp-zaro.zarr"))
  check_group_ds(open_dataset(s$path, group = "meas/r10", engine = "zaro"), s$band)
  check_group_ds(open_dataset(file.path(s$path, "meas", "r10")), s$band)
  root <- open_dataset(s$path, engine = "zaro")
  expect_equal(names(root@coords), "lon")
  expect_error(open_dataset(s$path, group = "nope", engine = "zaro"),
               "not in the store")
})

test_that("GDAL engine opens a group, by argument or in the path", {
  skip_if_not_installed("GDAL7")
  skip_if_not_installed("altarr")
  s <- make_nested(file.path(tempdir(), "grp-gdal.zarr"))
  check_group_ds(open_dataset(s$path, group = "meas/r10", engine = "gdal"), s$band)
  check_group_ds(open_dataset(file.path(s$path, "meas/r10"), engine = "gdal"),
                 s$band)
  expect_error(open_dataset(s$path, group = "meas/nope", engine = "gdal"),
               "not in")
})
