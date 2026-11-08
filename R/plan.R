# plan.R -- nd_plan(): what reading lazy data will cost, before reading it
#
# A lazy Variable from open_dataset() carries its source: the engine, the
# dataset and array, the array's chunk shape, and the selected positions
# along each of its dimensions (composed by isel()/sel()). From that,
# nd_plan() works out which chunks the read touches, and with the zaro
# engine asks zaro_plan() where their bytes are: requests, bytes, hosts.


#' Where a lazy Variable's data comes from
#'
#' @param be the Dataset backend.
#' @param var_name the variable.
#' @param schema its schema (dims and sizes, R order).
#' @param data the lazy array (for its chunk shape).
#' @keywords internal
#' @noRd
lazy_source <- function(be, var_name, schema, data) {
  dims <- schema$dim_names
  list(engine = if (is.null(be$engine)) "gdal" else be$engine,
       dsn = be$dsn, group = be$group, var = var_name,
       dims = dims, sizes = stats::setNames(as.integer(schema$dim_sizes), dims),
       chunk = stats::setNames(as.integer(lazy_chunk(data)), dims),
       # selected 1-based positions per source dim; NULL is all of it
       idx = stats::setNames(vector("list", length(dims)), dims))
}

#' A source after a selection
#'
#' @param src the source (or NULL).
#' @param full named list: the selected positions along each of the
#'   Variable's current dims, 1-based.
#' @keywords internal
#' @noRd
compose_source <- function(src, full) {
  if (is.null(src)) return(NULL)
  for (d in names(full)) {
    if (!d %in% src$dims) return(NULL)        # not a source dim: lost track
    cur <- src$idx[[d]]
    src$idx[[d]] <- as.integer(if (is.null(cur)) full[[d]] else cur[full[[d]]])
  }
  src
}


#' What reading lazy data will cost
#'
#' Works out what a read of lazy data (from [open_dataset()], after any
#' [sel()] or [isel()]) would touch, without reading it: the selection's
#' size, the source chunks it needs, and with the zaro engine the requests,
#' bytes, hosts and transports, from [zaro::zaro_plan()]. Call it before
#' [collect()] on something large or remote.
#'
#' With the GDAL engine the chunks are GDAL's block size and the requests
#' are not estimated (they depend on the driver). In-memory data has
#' nothing to read.
#'
#' @param x a Variable, DataArray or Dataset.
#' @param ... unused.
#' @param max_chunks the most chunks to plan byte references for: above
#'   this, only the counts are given (a whole large array can have millions
#'   of chunks). Default `getOption("ndr.plan_max_chunks", 1e5)`.
#' @return An `nd_plan` (printed as a summary). For a Variable or DataArray,
#'   a list with `var`, `engine`, `lazy`, `selection` (the selected sizes
#'   of the source dims), `values`, `chunks` (touched), `chunks_total`,
#'   `chunk_shape`, `chunk_values` (values in the touched chunks), and with
#'   the zaro engine `requests`, `bytes` (known), `unknown_size`, `hosts`,
#'   `transport` and `refs` (the [zaro::zaro_plan()] table). For a Dataset,
#'   a data frame with one row per data variable.
#' @examples
#' \dontrun{
#' ds <- open_dataset("https://host/store.zarr", engine = "zaro")
#' ds$temp |> sel(time = "2023-01-01") |> nd_plan()
#' nd_plan(ds)
#' }
#' @export
nd_plan <- new_generic("nd_plan", "x", function(x, ..., max_chunks =
                         getOption("ndr.plan_max_chunks", 1e5)) {
  S7_dispatch()
})

method(nd_plan, Variable) <- function(x, ..., max_chunks =
                                        getOption("ndr.plan_max_chunks", 1e5)) {
  plan_variable(x, name = NA_character_, max_chunks = max_chunks)
}

method(nd_plan, DataArray) <- function(x, ..., max_chunks =
                                         getOption("ndr.plan_max_chunks", 1e5)) {
  nm <- if (length(x@name)) x@name else NA_character_
  plan_variable(x@variable, name = nm, max_chunks = max_chunks)
}

method(nd_plan, Dataset) <- function(x, ..., max_chunks =
                                       getOption("ndr.plan_max_chunks", 1e5)) {
  be <- x@.backend
  nms <- c(names(x@data_vars), if (!is.null(be)) names(be$schemas))
  rows <- lapply(nms, function(nm) {
    v <- if (nm %in% names(x@data_vars)) x@data_vars[[nm]] else
      backend_lazy_var(be, nm)
    p <- plan_variable(v, name = nm, max_chunks = max_chunks)
    data.frame(var = nm, engine = p$engine, lazy = p$lazy, values = p$values,
               chunks = p$chunks,
               requests = if (is.null(p$requests)) NA_real_ else p$requests,
               bytes = if (is.null(p$bytes) || p$unknown_size == p$requests)
                 NA_real_ else p$bytes,
               hosts = paste(p$hosts, collapse = ", "),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) out <- data.frame(var = character())
  class(out) <- c("nd_plan_dataset", "data.frame")
  out
}


#' @keywords internal
#' @noRd
plan_variable <- function(v, name, max_chunks) {
  d <- var_data(v)
  src <- v@source
  shp <- shape(v)
  out <- list(var = name, engine = NA_character_, lazy = is_lazy(d),
              selection = shp, values = prod(as.double(shp)))
  if (!out$lazy || is.null(src)) {
    out$engine <- if (out$lazy) "unknown" else "memory"
    out$chunks <- 0
    return(structure(out, class = "nd_plan"))
  }
  out$engine <- src$engine
  sel_n <- vapply(src$dims, function(dn) {
    i <- src$idx[[dn]]
    if (is.null(i)) src$sizes[[dn]] else length(i)
  }, 1)
  out$selection <- stats::setNames(as.integer(sel_n), src$dims)
  out$values <- prod(sel_n)

  # chunk grid indices touched along each source dim (0-based, R order)
  cid <- lapply(src$dims, function(dn) {
    n <- src$sizes[[dn]]
    cs <- src$chunk[[dn]]
    i <- src$idx[[dn]]
    if (is.null(i)) seq.int(0L, (n - 1L) %/% cs) else sort(unique((i - 1L) %/% cs))
  })
  names(cid) <- src$dims
  ext <- Map(function(ids, dn) {
    cs <- src$chunk[[dn]]
    sum(pmin(cs, src$sizes[[dn]] - ids * cs))
  }, cid, src$dims)
  out$chunks <- prod(as.double(lengths(cid)))
  out$chunks_total <- prod(ceiling(as.double(src$sizes) / src$chunk))
  out$chunk_shape <- src$chunk
  out$chunk_values <- prod(as.double(unlist(ext)))

  if (identical(src$engine, "zaro") && out$chunks <= max_chunks) {
    out <- c(out, plan_zaro_refs(src, cid))
  }
  structure(out, class = "nd_plan")
}

#' zaro_plan() for the touched chunks, and its cost
#' @keywords internal
#' @noRd
plan_zaro_refs <- function(src, cid) {
  zd <- src$dsn
  meta <- zd$arrays[[src$var]]
  grid <- as.matrix(expand.grid(cid, KEEP.OUT.ATTRS = FALSE))
  storage.mode(grid) <- "integer"
  # R order -> Zarr order (C-order arrays are reversed)
  if (!identical(meta@raw_meta[["order"]], "F")) grid <- grid[, rev(seq_len(ncol(grid))), drop = FALSE]
  refs <- zaro::zaro_plan(zd, src$var, chunks = unname(grid), verbose = FALSE)
  fetch <- !refs$transport %in% c("inline", "absent")
  url <- refs$path[fetch]
  host <- ifelse(grepl("^[a-z0-9+]+://", url),
                 sub("^[a-z0-9+]+://([^/]+).*$", "\\1", url), NA_character_)
  list(requests = sum(fetch),
       bytes = sum(refs$size[fetch], na.rm = TRUE),
       unknown_size = sum(is.na(refs$size[fetch])),
       hosts = sort(unique(stats::na.omit(host))),
       transport = table(refs$transport),
       refs = refs)
}

#' @keywords internal
#' @noRd
format_size <- function(b) {
  if (is.na(b)) return("NA")
  u <- c("B", "kB", "MB", "GB", "TB")
  k <- if (b > 0) min(floor(log(b, 1000)), length(u) - 1L) else 0
  sprintf(if (k == 0) "%.0f %s" else "%.1f %s", b / 1000^k, u[k + 1L])
}

#' @keywords internal
#' @noRd
format_count <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

# Registered in .onLoad() against base's print: ndr's namespace holds its
# own `print` (from `method(print, ...) <-`), so an S3method() line, or S7's
# method() for an S3 class, registers these in ndr's table, where
# auto-printing never looks
print_nd_plan <- function(x, ...) {
  label <- if (is.na(x$var)) "" else paste0(" '", x$var, "'")
  cat(sprintf("<nd_plan>%s  [%s]\n", label,
              switch(x$engine, memory = "in memory", unknown = "lazy, source unknown",
                     paste(x$engine, "engine"))))
  sel <- paste(names(x$selection), x$selection, collapse = ", ")
  cat("  selection  ", sel, "\n", sep = "")
  cat("  values     ", format_count(x$values), " (", format_size(8 * x$values),
      " as double)\n", sep = "")
  if (x$engine %in% c("memory", "unknown")) {
    if (x$engine == "memory") cat("  reads      nothing (in memory)\n")
    return(invisible(x))
  }
  cat("  chunks     ", format_count(x$chunks), " of ",
      format_count(x$chunks_total), " (chunk ",
      paste(x$chunk_shape, collapse = " x "), "; ",
      format_count(x$chunk_values), " values decoded)\n", sep = "")
  if (is.null(x$requests)) {
    cat("  reads      ", if (x$engine == "gdal")
      "not estimated (GDAL's drivers decide the requests)" else
      sprintf("not planned: more than %s chunks (ndr.plan_max_chunks)",
              format_count(getOption("ndr.plan_max_chunks", 1e5))), "\n", sep = "")
    return(invisible(x))
  }
  parts <- c(if (x$requests > x$unknown_size) format_size(x$bytes),
             if (x$unknown_size) sprintf("%s whole object(s) of unknown size",
                                         format_count(x$unknown_size)))
  cat("  reads      ", format_count(x$requests), " request(s)",
      if (length(parts)) paste0(", ", paste(parts, collapse = " + ")), "\n", sep = "")
  if (length(x$hosts)) cat("  hosts      ", paste(x$hosts, collapse = ", "), "\n", sep = "")
  tr <- x$transport
  cat("  transport  ", paste(names(tr), tr, collapse = ", "), "\n", sep = "")
  invisible(x)
}

print_nd_plan_dataset <- function(x, ...) {
  cat("<nd_plan> Dataset:", nrow(x), "variable(s)\n")
  y <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  y$values <- vapply(y$values, format_count, "")
  y$bytes <- vapply(y$bytes, function(b) if (is.na(b)) "" else format_size(b), "")
  y$requests <- ifelse(is.na(y$requests), "", vapply(y$requests, format_count, ""))
  print(y, row.names = FALSE)
  invisible(x)
}
