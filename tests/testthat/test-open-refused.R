## Arrays a byterefs store refuses to decode (byterefs decision 0007)

lzma <- '[{"name":"bytes","configuration":{"endian":"little"}},{"name":"lzma"}]'

open_br <- function(...) {
  suppressWarnings(open_dataset(paste0("byterefs://", make_byterefs(...)),
                                target = "local", engine = "zaro"))
}

test_that("a refused data variable is listed with its reason, not offered", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ## zaro warns at open, naming the array and the reason
  expect_warning(open_dataset(paste0("byterefs://", make_byterefs(codec = lzma)),
                              target = "local", engine = "zaro"),
                 "v: unknown codec 'lzma'")
  ds <- open_br(codec = lzma)
  expect_false("v" %in% names(ds))
  expect_identical(names(ds@.backend$refused), "v")
  expect_error(ds$v, "can't read 'v': unknown codec 'lzma'")
  out <- capture.output(print(ds))
  i <- grep("Not readable", out)
  expect_length(i, 1L)
  expect_match(out[i + 1L], "v .*\\(x, y\\) unknown codec 'lzma'")
})

test_that("vars = naming a refused array is an error with the reason", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  expect_error(suppressWarnings(open_dataset(
    paste0("byterefs://", make_byterefs(codec = lzma)), target = "local",
    engine = "zaro", vars = "v")), "can't read 'v': unknown codec")
})

test_that("a refused coordinate doesn't stop the open; the rest reads", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("altarr")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ds <- open_br(refuse_y = TRUE)
  expect_false("y" %in% names(ds@coords))
  expect_identical(names(ds@.backend$refused), "y")
  expect_identical(names(ds), "v")
  v <- as.array(ds$v)                                # (x, y) = 6 x 2
  expect_equal(unname(v[1:4, ]), matrix(c(1:4, 5:8), 4))
  expect_true(all(is.na(v[5:6, ])))
})

test_that("a store that refuses nothing has no refused list", {
  skip_if_not_installed("zaro")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ds <- open_br()
  expect_length(ds@.backend$refused, 0L)
  expect_false(any(grepl("Not readable", capture.output(print(ds)))))
})
