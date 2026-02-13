#' Internal data access
#'
#' Bypasses S3 as.array dispatch (which breaks with S7 class naming)
#' by accessing @data directly and ensuring dim is set.
#'
#' @param x A Variable
#' @return An R array with dim set
#' @keywords internal
#' @noRd
var_data <- function(x) {
  d <- x@data
  if (is.null(dim(d)) && length(x@dims) == 1L) {
    dim(d) <- length(d)
  }
  d
}


#' Check if a value supports range comparison (>=, <=)
#'
#' TRUE for numeric, integer, Date, POSIXt, difftime, and anything
#' with an Ops method that handles comparison. Used by sel() to decide
#' whether a 2-element vector means "range" or "two specific values".
#'
#' @param x A vector
#' @return Logical
#' @keywords internal
#' @noRd
is_orderable <- function(x) {
  is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXt") || inherits(x, "difftime")
}
