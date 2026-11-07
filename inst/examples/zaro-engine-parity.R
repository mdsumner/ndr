# zaro-engine-parity.R -- open_dataset(engine = "zaro") against the GDAL
# engine on three public stores, and the BRAN2023 track against a reference
#
#   Rscript zaro-engine-parity.R           # all checks, needs network
#   Rscript zaro-engine-parity.R aodn      # one of: aodn, bran, track, cmip6
#
# Needs ndr, zaro, altarr, and GDAL7 for the GDAL side (skipped without it).
# The zaro engine never calls GDAL: S3/GCS go through Arrow, the VirtualiZarr
# manifests and THREDDS byte ranges through curl/Arrow.
#
# Comparison rule: values must be equal where both engines have a value, and
# the missing-value pattern is reported. GDAL returns NaN for missing floats
# where zaro returns NA; both count as missing.
#
# bran-track-reference.csv is track_real.csv from zaro's
# inst/examples/track-extract (a real run against NCI THREDDS, 2026-10-03):
# 1-based (i, j, k, l) indices into temp (xt_ocean, yt_ocean, st_ocean, Time)
# and the CF-decoded temperature. ASCII only.

suppressPackageStartupMessages(library(ndr))
options(zaro.verbose = FALSE)

args <- commandArgs(trailingOnly = TRUE)
which_checks <- if (length(args)) args else c("aodn", "bran", "track", "cmip6")
here <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) dirname(sub("^--file=", "", f[1])) else
    system.file("examples", package = "ndr")
})
has_gdal <- requireNamespace("GDAL7", quietly = TRUE)

results <- list()
record <- function(check, ...) {
  results[[length(results) + 1L]] <<- data.frame(check = check, ...)
}

#' compare two numeric arrays: same missing pattern? max |diff| where both set
compare <- function(check, z, g, tz, tg) {
  z <- as.vector(z); g <- as.vector(g)
  both <- !is.na(z) & !is.na(g)
  record(check, n = length(z), na_zaro = sum(is.na(z)), na_gdal = sum(is.na(g)),
         same_na = identical(is.na(z), is.na(g)),
         max_abs_diff = if (any(both)) max(abs(z[both] - g[both])) else NA_real_,
         sec_zaro = round(tz, 2), sec_gdal = round(tg, 2))
}

timed <- function(expr) {
  t0 <- Sys.time()
  v <- force(expr)
  list(value = v, sec = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

values <- function(da) as.vector(collect(da)@variable@data)

## --- AODN gridded sea level anomaly: Zarr V2 on S3 -------------------------
if ("aodn" %in% which_checks) {
  src <- "s3://aodn-cloud-optimised/model_sea_level_anomaly_gridded_realtime.zarr"
  z <- open_dataset(src, vars = "GSLA", anonymous = TRUE,
                    region = "ap-southeast-2")
  tt <- coord_values(z@coords$TIME)
  pick <- tt[c(1L, length(tt) %/% 2L)]
  zv <- lapply(pick, function(t) timed(values(sel(z$GSLA, TIME = t))))
  if (has_gdal) {
    Sys.setenv(AWS_NO_SIGN_REQUEST = "YES", AWS_REGION = "ap-southeast-2")
    g <- open_dataset(sprintf('ZARR:"/vsis3/%s"', sub("^s3://", "", src)),
                      vars = "GSLA", engine = "gdal")
    for (k in seq_along(pick)) {
      gv <- timed(values(sel(g$GSLA, TIME = pick[k])))
      compare(paste("aodn GSLA", format(pick[k])), zv[[k]]$value, gv$value,
              zv[[k]]$sec, gv$sec)
    }
  }
}

## --- BRAN2023 temp: VirtualiZarr Parquet, chunks on NCI THREDDS ------------
bran_src <- "https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq"
if ("bran" %in% which_checks) {
  z <- open_dataset(paste0("virtualizarr://", bran_src), vars = "temp")
  win <- function(ds) {
    sel(ds$temp, st_ocean = 2.5, Time = coord_values(ds@coords$Time)[1],
        xt_ocean = c(140, 150), yt_ocean = c(-45, -40))
  }
  zv <- timed(values(win(z)))
  if (has_gdal) {
    g <- open_dataset(sprintf('ZARR:"/vsicurl/%s"', bran_src), vars = "temp",
                      engine = "gdal")
    gv <- timed(values(win(g)))
    compare("bran temp window", zv$value, gv$value, zv$sec, gv$sec)
    land <- is.na(zv$value) & !is.na(gv$value)
    if (any(land)) {
      cat(sprintf(paste0("bran: %d cells are NA in zaro (_FillValue -32768) ",
                         "but %s in GDAL, which does not mask them\n"),
                  sum(land), paste(unique(round(gv$value[land], 4)),
                                   collapse = ", ")))
    }
  }
}

## --- BRAN2023 track: one planned point read, against the reference --------
if ("track" %in% which_checks) {
  ref <- utils::read.csv(file.path(here, "bran-track-reference.csv"))
  z <- open_dataset(paste0("virtualizarr://", bran_src), vars = "temp")
  x <- z$temp@variable@data      # lazy altarr array (xt, yt, st, Time)
  tv <- timed(x[cbind(ref$i, ref$j, ref$k, ref$l)])
  record("bran track (30 points)", n = nrow(ref),
         na_zaro = sum(is.na(tv$value)), na_gdal = NA_integer_,
         same_na = !anyNA(tv$value),
         max_abs_diff = max(abs(tv$value - ref$temp)),
         sec_zaro = round(tv$sec, 2), sec_gdal = NA_real_)
}

## --- CMIP6 GFDL-ESM4 tas: Pangeo Zarr V2 on GCS ----------------------------
if ("cmip6" %in% which_checks) {
  src <- "gs://cmip6/CMIP6/CMIP/NOAA-GFDL/GFDL-ESM4/historical/r1i1p1f1/Amon/tas/gr1/v20190726"
  z <- open_dataset(src, vars = "tas")
  tt <- coord_values(z@coords$time)
  pick <- tt[c(1L, length(tt))]
  zv <- lapply(pick, function(t) timed(values(sel(z$tas, time = t))))
  if (has_gdal) {
    Sys.setenv(GS_NO_SIGN_REQUEST = "YES")
    g <- open_dataset(sprintf('ZARR:"/vsigs/%s"', sub("^gs://", "", src)),
                      vars = "tas", engine = "gdal")
    stopifnot(identical(format(coord_values(g@coords$time)[c(1L, length(tt))]),
                        format(pick)))
    for (k in seq_along(pick)) {
      gv <- timed(values(sel(g$tas, time = pick[k])))
      compare(paste("cmip6 tas", format(pick[k])), zv[[k]]$value, gv$value,
              zv[[k]]$sec, gv$sec)
    }
  }
}

out <- do.call(rbind, results)
print(out, row.names = FALSE)
invisible(out)
