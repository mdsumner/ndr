## shared by test-open-zaro.R and test-open-group.R

## write an uncompressed V2 array (C order) given in R dim order
write_v2 <- function(path, x, chunk, dims, attrs = NULL, dtype = "<f8",
                     fill = "NaN") {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  d <- dim(x)
  if (is.null(d)) d <- length(x)
  x <- array(x, d)
  jarr <- function(v) paste0("[", paste(v, collapse = ", "), "]")
  jstr <- function(v) paste0("[", paste0('"', v, '"', collapse = ", "), "]")
  writeLines(sprintf(
    '{"zarr_format": 2, "shape": %s, "chunks": %s, "dtype": "%s", "compressor": null, "fill_value": %s, "filters": null, "order": "C"}',
    jarr(rev(d)), jarr(rev(chunk)), dtype, fill), file.path(path, ".zarray"))
  a <- c(sprintf('"_ARRAY_DIMENSIONS": %s', jstr(rev(dims))), attrs)
  writeLines(paste0("{", paste(a, collapse = ", "), "}"),
             file.path(path, ".zattrs"))
  is_int <- dtype == "<i2"
  grid <- lapply(ceiling(d / chunk) - 1L, function(n) seq.int(0L, n))
  cc_all <- as.matrix(expand.grid(grid, KEEP.OUT.ATTRS = FALSE))
  for (r in seq_len(nrow(cc_all))) {
    cc <- as.integer(cc_all[r, ])
    st <- cc * chunk + 1L
    ext <- pmin(chunk, d - st + 1L)
    region <- do.call(`[`, c(list(x), Map(seq.int, st, length.out = ext),
                             list(drop = FALSE)))
    padded <- array(if (is_int) -32768L else NaN, chunk)
    padded <- do.call(`[<-`, c(list(padded), lapply(ext, seq_len),
                               list(value = region)))
    bytes <- if (is_int) {
      writeBin(as.integer(padded), raw(), size = 2L, endian = "little")
    } else {
      writeBin(as.double(padded), raw(), size = 8L, endian = "little")
    }
    writeBin(bytes, file.path(path, paste(rev(cc), collapse = ".")))
  }
}
