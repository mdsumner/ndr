#' Open a dataset from a file or URL
#'
#' Read a multidimensional data source into an ndr Dataset. Currently supports
#' any source that GDAL's multidim API can read: NetCDF, HDF5, Zarr v2/v3,
#' kerchunk-parquet virtual stores, and VRT multidim. Works with local paths,
#' `/vsicurl/`, `/vsis3/`, and other GDAL virtual filesystems.
#'
#' Requires the GDAL7 package (`remotes::install_github("rgdal-dev/GDAL7")`,
#' GDAL >= 3.10) and, for reading data, the altarr package
#' (`remotes::install_github("hypertidy/altarr")`).
#'
#' @param dsn Data source name. A file path, URL, or GDAL connection string
#'   (e.g. `'ZARR:"/vsicurl/https://example.com/store.parq"'`).
#' @param vars Character vector of variable names to include. Default `NULL`
#'   includes all data variables. Use `character()` for schema + coords only.
#'   All variables are loaded lazily on first access via `$`.
#' @param ... Reserved for future use.
#'
#' @return A [Dataset] with coordinates, global attributes, and lazy data
#'   variables whose values are read only when used.
#'
#' @details
#'
#' ## Lazy loading
#'
#' `open_dataset()` reads only coordinates and metadata. Accessing a data
#' variable via `ds$var_name` returns a [DataArray] whose data is a lazy
#' chunked array (see [lazy-data]): still no array data is read. `sel()` and
#' `isel()` stay lazy, reductions stream the array chunk by chunk, and
#' arithmetic or [collect()] read the selected values. This allows opening
#' large datasets (e.g. 12TB BRAN2023) without reading any array data. Use
#' `vars` to limit which variables are available. Lazy reads need the altarr
#' package (`remotes::install_github("hypertidy/altarr")`).
#'
#' ## Variable classification
#'
#' Arrays are classified as coordinates or data variables based on CF
#' conventions: a 1D array whose name matches its dimension name is treated
#' as a coordinate. All other arrays with >1 dimension are data variables.
#' Bounds arrays (e.g. `time_bnds`) and scalar arrays are skipped.
#'
#' ## Coordinate types
#'
#' Regular spatial grids (equal spacing within floating-point tolerance) are
#' stored as [ImplicitCoord] (offset + step, no data allocation). Irregular
#' grids and time coordinates are stored as [ExplicitCoord].
#'
#' ## CF time decoding
#'
#' Time dimensions (GDAL type "TEMPORAL") are automatically decoded from
#' their CF units (e.g. "days since 1800-01-01") to R Date or POSIXct values
#' using [cf_decode_time()].
#'
#' ## Dimension ordering
#'
#' Arrays are stored in R's column-major (Fortran) order, matching GDAL7's `read_mdarray()`
#' `$gis$dim` convention. Dimension names follow the same order. For a NetCDF
#' variable with dimensions (time, lat, lon), the R array has
#' `dim = c(nlon, nlat, ntime)` and `dims = c("lon", "lat", "time")`.
#'
#' @examples
#' \dontrun{
#' # Open lazily - no data read yet
#' ds <- open_dataset("sst.mnmean.nc")
#' ds  # shows variables with [not loaded]
#'
#' # Selecting is lazy too; collect() reads just the selected values
#' ds$sst |> sel(time = as.Date("2020-06-15"), lat = c(-60, -30)) |> collect()
#'
#' # Reductions stream the array chunk by chunk
#' ds$sst |> sel(lat = c(-60, -30)) |> nd_mean("time", na.rm = TRUE)
#'
#' # Scope to specific variables (still lazy)
#' ds <- open_dataset("sst.mnmean.nc", vars = "sst")
#'
#' # Remote kerchunk-parquet - only sst schema, 12TB never touched
#' dsn <- 'ZARR:"/vsicurl/https://example.com/store.parq"'
#' ds <- open_dataset(dsn, vars = "temp")
#' ds$temp  # a lazy DataArray: nothing read yet
#' }
#'
#' @export
open_dataset <- function(dsn, vars = NULL, ...) {
  check_gdal7()
  open_dataset_gdal(dsn, vars = vars, ...)
}


#' @keywords internal
#' @noRd
open_dataset_gdal <- function(dsn, vars = NULL, ...) {

  ds <- GDAL7::gdal_open(dsn, multidim = TRUE)
  on.exit(GDAL7::gdal_close(ds), add = TRUE)
  root <- GDAL7::get_root_group(ds)
  if (is.null(root)) {
    stop(sprintf("'%s' is not a multidimensional source", dsn), call. = FALSE)
  }

  array_names <- root@mdarray_names

  # --- Phase 1: classify arrays as coords or data vars ---
  coord_names <- character()
  data_var_names <- character()
  arrays <- list()  # open arrays, reused below

  for (nm in array_names) {
    arr <- GDAL7::open_mdarray(root, nm)
    if (is.null(arr)) next
    arrays[[nm]] <- arr
    dim_names <- arr@dimensions$name

    ndims <- length(dim_names)
    if (ndims == 0L) next  # skip scalar arrays
    if (ndims == 1L && dim_names == nm) {
      coord_names <- c(coord_names, nm)
    } else if (ndims > 1L) {
      data_var_names <- c(data_var_names, nm)
    }
    # skip: 1D arrays that don't match their dim name (bounds, auxiliary)
  }

  # --- Phase 2: build coordinates (always read) ---
  coords <- list()
  for (nm in coord_names) {
    arr <- arrays[[nm]]
    vals <- GDAL7::read_mdarray(arr)
    arr_attrs <- arr@attributes

    # CF time decode: GDAL gives CF units as the array's unit (netCDF tags
    # the dimension TEMPORAL; Zarr does not), else look in the attributes
    time_units <- NULL
    for (u in list(arr@unit_type, arr_attrs[["units"]])) {
      if (is.character(u) && length(u) == 1L && grepl("since", u, fixed = TRUE)) {
        time_units <- u
        break
      }
    }

    if (!is.null(time_units)) {
      vals <- tryCatch(
        cf_decode_time(vals, time_units, arr_attrs[["calendar"]]),
        error = function(e) vals  # fall back to raw numeric
      )
    }

    # Choose coord type
    if (is.numeric(vals) && length(vals) >= 2L && is_regular(vals)) {
      coords[[nm]] <- ImplicitCoord(
        dimension = nm,
        n         = length(vals),
        offset    = vals[1L],
        step      = regular_step(vals)
      )
    } else {
      coords[[nm]] <- ExplicitCoord(dimension = nm, values = vals)
    }
  }

  # --- Phase 3: determine scope ---
  # vars = NULL       -> all data vars (lazy)
  # vars = "sst"      -> only sst (lazy)
  # vars = character() -> none (schema + coords only)
  if (!is.null(vars) && length(vars) > 0L) {
    missing <- setdiff(vars, data_var_names)
    if (length(missing) > 0L) {
      stop(sprintf(
        "requested variable(s) not found: %s\navailable: %s",
        paste(missing, collapse = ", "),
        paste(data_var_names, collapse = ", ")
      ))
    }
    data_var_names <- vars
  } else if (!is.null(vars) && length(vars) == 0L) {
    data_var_names <- character()
  }

  # --- Phase 4: build schemas for lazy variables ---
  schemas <- list()
  for (nm in data_var_names) {
    arr <- arrays[[nm]]
    dims <- arr@dimensions

    # attributes come back all at once (cheap, just metadata)
    arr_attrs <- arr@attributes
    unit <- arr@unit_type
    if (is.null(arr_attrs[["units"]]) && length(unit) == 1L && nzchar(unit)) {
      arr_attrs[["units"]] <- unit
    }

    schemas[[nm]] <- list(
      dim_names = rev(dims$name),
      dim_sizes = as.integer(rev(dims$size)),
      attrs     = arr_attrs
    )
  }

  # --- Phase 5: global attributes ---
  global_attrs <- tryCatch(root@attributes, error = function(e) list())

  # --- Build backend (if there are lazy vars) ---
  backend <- NULL
  if (length(schemas) > 0L) {
    backend <- list(
      dsn     = dsn,
      schemas = schemas,
      cache   = new.env(parent = emptyenv())
    )
  }

  Dataset(
    data_vars = list(),
    coords    = coords,
    attrs     = global_attrs,
    .backend  = backend
  )
}


#' A lazy Variable for a backend variable, built once and cached
#'
#' Only the array's metadata is read here; values are read by the altarr
#' array's fetch function when they are asked for.
#' @keywords internal
#' @noRd
backend_lazy_var <- function(be, var_name) {
  if (exists(var_name, envir = be$cache, inherits = FALSE)) {
    return(get(var_name, envir = be$cache, inherits = FALSE))
  }
  check_altarr()
  schema <- be$schemas[[var_name]]
  v <- Variable(
    dims  = schema$dim_names,
    data  = gdal_lazy_array(be$dsn, var_name),
    attrs = schema$attrs
  )
  assign(var_name, v, envir = be$cache)
  v
}


# --- GDAL7 availability helpers ---

#' @keywords internal
#' @noRd
check_gdal7 <- function() {
  if (!requireNamespace("GDAL7", quietly = TRUE)) {
    stop(
      "GDAL7 package is required for open_dataset().\n",
      "Install with: remotes::install_github(\"rgdal-dev/GDAL7\")",
      call. = FALSE
    )
  }
}

#' @keywords internal
#' @noRd
check_altarr <- function() {
  if (!requireNamespace("altarr", quietly = TRUE)) {
    stop(
      "altarr package is required for lazy reads from open_dataset().\n",
      "Install with: remotes::install_github(\"hypertidy/altarr\")",
      call. = FALSE
    )
  }
}
