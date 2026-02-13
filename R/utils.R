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
