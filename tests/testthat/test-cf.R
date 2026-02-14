test_that("cf_parse_time_units parses standard formats", {
  p <- cf_parse_time_units("days since 1800-01-01")
  expect_equal(p$unit, "days")
  expect_s3_class(p$origin, "POSIXct")
  expect_equal(as.Date(p$origin), as.Date("1800-01-01"))

  p2 <- cf_parse_time_units("hours since 1979-01-01 00:00:00")
  expect_equal(p2$unit, "hours")
  expect_equal(as.Date(p2$origin), as.Date("1979-01-01"))
})

test_that("cf_parse_time_units handles non-zero-padded dates", {
  # NetCDF files often have "1800-1-1" instead of "1800-01-01"
  p <- cf_parse_time_units("days since 1800-1-1 00:00:00")
  expect_equal(p$unit, "days")
  expect_equal(as.Date(p$origin), as.Date("1800-01-01"))
})

test_that("cf_parse_time_units rejects invalid strings", {
  expect_error(cf_parse_time_units(""), "empty or NULL")
  expect_error(cf_parse_time_units(NULL), "empty or NULL")
  expect_error(cf_parse_time_units("days"), "cannot parse CF time units")
  expect_error(cf_parse_time_units("days from 1800-01-01"), "cannot parse CF time units")
})

test_that("cf_decode_time decodes days since", {
  dates <- cf_decode_time(c(0, 1, 365), "days since 2020-01-01")
  expect_s3_class(dates, "Date")
  expect_equal(dates[1], as.Date("2020-01-01"))
  expect_equal(dates[2], as.Date("2020-01-02"))
  expect_equal(dates[3], as.Date("2021-01-01"))  # 2020 is leap year
})

test_that("cf_decode_time decodes hours since", {
  times <- cf_decode_time(c(0, 24, 48), "hours since 2020-01-01 00:00:00")
  expect_s3_class(times, "POSIXct")
  expect_equal(as.Date(times[1]), as.Date("2020-01-01"))
  expect_equal(as.Date(times[2]), as.Date("2020-01-02"))
  expect_equal(as.Date(times[3]), as.Date("2020-01-03"))
})

test_that("cf_decode_time decodes seconds since", {
  times <- cf_decode_time(c(0, 3600), "seconds since 2020-01-01 00:00:00")
  expect_s3_class(times, "POSIXct")
  expect_equal(difftime(times[2], times[1], units = "hours"), as.difftime(1, units = "hours"))
})

test_that("cf_decode_time rejects unknown units", {
  expect_error(cf_decode_time(1, "fortnights since 2020-01-01"), "unsupported CF time unit")
})

test_that("cf_decode_time handles OISST-style units", {
  # Real-world: "days since 1800-1-1 00:00:00"
  vals <- c(66443, 66474)
  dates <- cf_decode_time(vals, "days since 1800-1-1 00:00:00")
  expect_s3_class(dates, "Date")
  # 66443 days after 1800-01-01 should be 1981-12-01
  expect_equal(dates[1], as.Date("1981-12-01"))
})


# --- is_regular tests ---

test_that("is_regular detects regular sequences", {
  expect_true(is_regular(1:10))
  expect_true(is_regular(seq(-89.5, 89.5, by = 1)))
  expect_true(is_regular(seq(0.5, 359.5, by = 1)))
  expect_true(is_regular(c(10, 20)))
  expect_true(is_regular(42))  # single value
})

test_that("is_regular detects irregular sequences", {
  expect_false(is_regular(c(1, 2, 4)))
  expect_false(is_regular(c(0, 1, 3, 6, 10)))
  expect_false(is_regular(c(1, 1, 1)))  # step = 0
})

test_that("is_regular handles edge cases", {
  expect_true(is_regular(numeric(0)))   # empty
  expect_true(is_regular(5.0))          # length 1
})

test_that("is_regular handles floating-point precision", {
  # Accumulated floating point drift over many steps
  x <- cumsum(rep(0.1, 100))
  expect_true(is_regular(x))
})

test_that("regular_step returns correct step", {
  expect_equal(regular_step(seq(0.5, 359.5, by = 1.0)), 1.0)
  expect_equal(regular_step(c(-89.5, -88.5, -87.5)), 1.0)
  expect_equal(regular_step(c(10, 5, 0)), -5)
})
