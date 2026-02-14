#' CF Convention Time Decoding
#'
#' Parse CF convention time unit strings and decode numeric values
#' to R Date or POSIXct objects.
#'
#' @name cf-time
#' @keywords internal
NULL

#' Parse a CF time units string
#'
#' Parses strings like "days since 1800-01-01" or
#' "hours since 1979-01-01 00:00:00" into components.
#'
#' @param units Character string in CF format: "<unit> since <reference>"
#' @return A list with `unit` (character) and `origin` (POSIXct)
#' @keywords internal
#' @noRd
cf_parse_time_units <- function(units) {
  if (is.null(units) || !nzchar(units))
    stop("empty or NULL CF time units string")

  parts <- strsplit(trimws(units), "\\s+since\\s+", perl = TRUE)[[1L]]
  if (length(parts) != 2L)
    stop(sprintf("cannot parse CF time units: '%s'", units))

  unit <- tolower(trimws(parts[1L]))
  ref_str <- trimws(parts[2L])

  # Normalise reference string — handle "1800-1-1" as well as "1800-01-01"
  # Try POSIXct first (handles datetime refs), fall back to Date
  origin <- tryCatch(
    as.POSIXct(ref_str, tz = "UTC"),
    error = function(e) {
      as.POSIXct(as.Date(ref_str), tz = "UTC")
    }
  )

  if (is.na(origin))
    stop(sprintf("cannot parse CF time origin: '%s'", ref_str))

  list(unit = unit, origin = origin)
}

#' Decode CF time values to R Date or POSIXct
#'
#' Converts numeric offset values to R Date or POSIXct using CF convention
#' time units and calendar. For standard/gregorian/proleptic_gregorian
#' calendars (and NULL, the common case), uses base R date arithmetic.
#' For non-standard calendars (360_day, noleap/365_day, all_leap/366_day),
#' delegates to the CFtime package if available.
#'
#' @param values Numeric vector of time values (e.g. days since origin)
#' @param units CF time units string (e.g. "days since 1800-01-01")
#' @param calendar CF calendar type. NULL or "standard"/"gregorian"/
#'   "proleptic_gregorian" use base R. Non-standard calendars require CFtime.
#' @return Date vector (for whole-day units on standard calendars),
#'   POSIXct (for sub-day units), or character timestamps (non-standard
#'   calendars via CFtime).
#'
#' @examples
#' # OISST: days since 1800-01-01 (standard calendar)
#' cf_decode_time(c(66443, 66474), "days since 1800-1-1 00:00:00")
#'
#' # BRAN: days since 1979-01-01
#' cf_decode_time(c(11323, 11324), "days since 1979-01-01")
#'
#' @export
cf_decode_time <- function(values, units, calendar = NULL) {
  cal <- if (is.null(calendar)) "standard" else tolower(trimws(calendar))

  # Standard calendars: base R arithmetic is correct
  if (cal %in% c("standard", "gregorian", "proleptic_gregorian", "")) {
    return(cf_decode_standard(values, units))
  }

  # Non-standard calendars: delegate to CFtime
  if (requireNamespace("CFtime", quietly = TRUE)) {
    return(cf_decode_cftime(values, units, cal))
  }

  # CFtime not available: warn and return raw numeric
  warning(sprintf(
    paste0(
      "non-standard calendar '%s' requires the CFtime package.\n",
      "Install with: install.packages(\"CFtime\")\n",
      "Returning raw numeric time values."
    ),
    calendar
  ))
  values
}


#' Decode standard calendar times with base R
#' @keywords internal
#' @noRd
cf_decode_standard <- function(values, units) {
  parsed <- cf_parse_time_units(units)

  seconds_per <- switch(parsed$unit,
    "days"    = , "day"    = 86400,
    "hours"   = , "hour"   = 3600,
    "minutes" = , "minute" = 60,
    "seconds" = , "second" = 1,
    stop(sprintf("unsupported CF time unit: '%s'", parsed$unit))
  )

  posix_vals <- parsed$origin + values * seconds_per

  # Return Date if units are whole days
  if (parsed$unit %in% c("days", "day")) {
    as.Date(posix_vals, tz = "UTC")
  } else {
    posix_vals
  }
}


#' Decode non-standard calendar times via CFtime package
#' @keywords internal
#' @noRd
cf_decode_cftime <- function(values, units, calendar) {
  cf <- CFtime::CFtime(units, calendar, values)
  timestamps <- CFtime::as_timestamp(cf)
  # Try to parse as Date first (most climate data is daily or coarser)
  dates <- tryCatch(as.Date(timestamps), error = function(e) NULL)
  if (!is.null(dates) && !anyNA(dates)) return(dates)
  # Fall back to POSIXct
  posix <- tryCatch(as.POSIXct(timestamps, tz = "UTC"), error = function(e) NULL)
  if (!is.null(posix) && !anyNA(posix)) return(posix)
  # Last resort: return as character timestamps
  timestamps
}


#' Detect whether a numeric vector is regularly spaced
#'
#' Returns TRUE if all differences between consecutive elements are equal
#' (within floating-point tolerance). Used to decide between ImplicitCoord
#' and ExplicitCoord when building coordinates from data sources.
#'
#' @param x A numeric vector (length >= 2)
#' @param tol Relative tolerance for comparison
#' @return Logical scalar
#' @keywords internal
#' @noRd
is_regular <- function(x, tol = 1e-8) {
  if (length(x) < 2L) return(TRUE)
  d <- diff(as.numeric(x))
  step <- d[1L]
  if (step == 0) return(FALSE)
  all(abs(d - step) <= abs(step) * tol)
}

#' Get the step size of a regular sequence
#'
#' @param x A numeric vector (length >= 2, assumed regular)
#' @return Numeric scalar
#' @keywords internal
#' @noRd
regular_step <- function(x) {
  diff(as.numeric(x[1:2]))
}
