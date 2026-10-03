#' Open a dataset from a file or URL
#'
#' Read a multidimensional data source into an ndr Dataset. Currently supports
#' any source that GDAL's multidim API can read: NetCDF, HDF5, Zarr v2/v3,
#' kerchunk-parquet virtual stores, and VRT multidim. Works with local paths,
#' `/vsicurl/`, `/vsis3/`, and other GDAL virtual filesystems.
#'
#' Requires the gdalraster package (>= 1.12.0) with multidim API support
#' (install from: `remotes::install_github("mdsumner/gdalraster@gdalmultidim-api")`).
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
#' Arrays are stored in R's column-major (Fortran) order, matching gdalraster's
#' `$gis$dim` convention. Dimension names follow the same order. For a NetCDF
#' variable with dimensions (time, lat, lon), the R array has
#' `dim = c(nlon, nlat, ntime)` and `dims = c("lon", "lat", "time")`.
#'
#' @examples
#' \dontrun{
#' # Open lazily — no data read yet
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
#' # Remote kerchunk-parquet — only sst schema, 12TB never touched
#' dsn <- 'ZARR:"/vsicurl/https://example.com/store.parq"'
#' ds <- open_dataset(dsn, vars = "temp")
#' ds$temp  # a lazy DataArray: nothing read yet
#' }
#'
#' @export
open_dataset <- function(dsn, vars = NULL, ...) {
  check_gdalraster()
  open_dataset_gdal(dsn, vars = vars, ...)
}


#' @keywords internal
#' @noRd
open_dataset_gdal <- function(dsn, vars = NULL, ...) {

  ds <- new(
    gdalraster_class("GDALMultiDimRaster"),
    dsn, TRUE, character(), FALSE
  )
  on.exit(ds$close(), add = TRUE)

  array_names <- ds$getArrayNames()

  # --- Phase 1: classify arrays as coords or data vars ---
  coord_names <- character()
  data_var_names <- character()
  var_infos <- list()  # cache array info for all vars

  for (nm in array_names) {
    arr <- ds$openArrayFromFullname(paste0("/", nm), character())
    info <- gdalraster_fn("mdim_array_info")(arr)
    var_infos[[nm]] <- info

    ndims <- length(info$dim_names)
    if (ndims == 0L) next  # skip scalar arrays
    if (ndims == 1L && info$dim_names == nm) {
      coord_names <- c(coord_names, nm)
    } else if (ndims > 1L) {
      data_var_names <- c(data_var_names, nm)
    }
    # skip: 1D arrays that don't match their dim name (bounds, auxiliary)
  }

  # --- Phase 2: build coordinates (always read) ---
  coords <- list()
  for (nm in coord_names) {
    arr <- ds$openArrayFromFullname(paste0("/", nm), character())
    vals <- gdalraster_fn("mdim_dim_values")(arr, 0L)
    ci <- gdalraster_fn("mdim_coord_info")(arr, 0L)

    # CF time decode — check dimension metadata first, then array attrs,
    # then mdim_array_info()$unit (GDAL Zarr driver puts it there)
    time_units <- NULL
    time_calendar <- NULL

    if (!is.null(ci$type) && ci$type == "TEMPORAL" && !is.null(ci$units)) {
      # GDAL tagged this as temporal (NetCDF driver does this)
      time_units <- ci$units
      time_calendar <- ci$calendar
    } else {
      # Zarr/HDF5: check array attributes for CF units like "days since ..."
      attr_names <- tryCatch(
        gdalraster_fn("mdim_array_attr_names")(arr),
        error = function(e) character()
      )
      if ("units" %in% attr_names) {
        u <- tryCatch(
          gdalraster_fn("mdim_array_attr")(arr, "units"),
          error = function(e) NULL
        )
        if (is.character(u) && length(u) == 1L && grepl("since", u, fixed = TRUE)) {
          time_units <- u
        }
      }
      # Also check mdim_array_info()$unit — GDAL Zarr driver exposes CF
      # units here rather than as an attribute
      if (is.null(time_units)) {
        info <- var_infos[[nm]]
        if (!is.null(info$unit) && nzchar(info$unit) && grepl("since", info$unit, fixed = TRUE)) {
          time_units <- info$unit
        }
      }
      # Calendar from attrs (even if units came from info$unit)
      if (!is.null(time_units) && "calendar" %in% attr_names) {
        time_calendar <- tryCatch(
          gdalraster_fn("mdim_array_attr")(arr, "calendar"),
          error = function(e) NULL
        )
      }
    }

    if (!is.null(time_units)) {
      vals <- tryCatch(
        cf_decode_time(vals, time_units, time_calendar),
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
  # vars = NULL       → all data vars (lazy)
  # vars = "sst"      → only sst (lazy)
  # vars = character() → none (schema + coords only)
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
    info <- var_infos[[nm]]
    dim_sizes <- as.integer(rev(info$shape))
    dim_names <- rev(info$dim_names)

    # Read attrs (cheap, just metadata)
    arr <- ds$openArrayFromFullname(paste0("/", nm), character())
    arr_attrs <- list()
    attr_names <- gdalraster_fn("mdim_array_attr_names")(arr)
    for (a in attr_names) {
      arr_attrs[[a]] <- tryCatch(
        gdalraster_fn("mdim_array_attr")(arr, a),
        error = function(e) NULL
      )
    }
    if (is.null(arr_attrs[["units"]]) && !is.null(info$unit) && nzchar(info$unit)) {
      arr_attrs[["units"]] <- info$unit
    }

    schemas[[nm]] <- list(
      dim_names = dim_names,
      dim_sizes = dim_sizes,
      attrs     = arr_attrs
    )
  }

  # --- Phase 5: global attributes ---
  global_attrs <- tryCatch({
    root <- ds$getRootGroup()
    gdalraster_fn("mdim_group_attrs")(root)
  }, error = function(e) list())

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


# --- gdalraster availability helpers ---

#' @keywords internal
#' @noRd
check_gdalraster <- function() {
  if (!requireNamespace("gdalraster", quietly = TRUE)) {
    stop(
      "gdalraster package is required for open_dataset().\n",
      "Install with: remotes::install_github(\"mdsumner/gdalraster@gdalmultidim-api\")",
      call. = FALSE
    )
  }
  # Check for multidim API
  if (!exists("mdim_array_read", envir = asNamespace("gdalraster"))) {
    stop(
      "gdalraster is installed but lacks multidim API support.\n",
      "Install the multidim branch: remotes::install_github(\"mdsumner/gdalraster@gdalmultidim-api\")",
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

#' Get a gdalraster Rcpp class
#' @keywords internal
#' @noRd
gdalraster_class <- function(name) {
  get(name, envir = asNamespace("gdalraster"))
}

#' Get a gdalraster function by name
#' @keywords internal
#' @noRd
gdalraster_fn <- function(name) {
  get(name, envir = asNamespace("gdalraster"))
}
