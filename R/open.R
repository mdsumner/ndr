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
#'   variables that load on first access.
#'
#' @details
#'
#' ## Lazy loading
#'
#' `open_dataset()` reads only coordinates and metadata. Data variables are
#' loaded on demand when accessed via `ds$var_name`, then cached for reuse.
#' This allows opening large datasets (e.g. 12TB BRAN2023) without reading
#' any array data. Use `vars` to limit which variables are available.
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
#' # Access triggers read
#' ds$sst |> sel(time = as.Date("2020-06-15"), lat = c(-60, -30))
#'
#' # Scope to specific variables (still lazy)
#' ds <- open_dataset("sst.mnmean.nc", vars = "sst")
#'
#' # Remote kerchunk-parquet — only sst schema, 12TB never touched
#' dsn <- 'ZARR:"/vsicurl/https://example.com/store.parq"'
#' ds <- open_dataset(dsn, vars = "temp")
#' ds$temp  # reads only temp, on demand
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

    # CF time decode
    if (!is.null(ci$type) && ci$type == "TEMPORAL" && !is.null(ci$units)) {
      vals <- tryCatch(
        cf_decode_time(vals, ci$units, ci$calendar),
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


#' Read a single variable from GDAL multidim
#' @keywords internal
#' @noRd
read_var_gdal <- function(ds_handle, var_name) {
  arr <- ds_handle$openArrayFromFullname(paste0("/", var_name), character())
  x <- gdalraster_fn("mdim_array_read")(arr)
  gis <- attr(x, "gis")

  # Build attrs
  arr_attrs <- list()
  attr_names <- gdalraster_fn("mdim_array_attr_names")(arr)
  for (a in attr_names) {
    arr_attrs[[a]] <- tryCatch(
      gdalraster_fn("mdim_array_attr")(arr, a),
      error = function(e) NULL
    )
  }
  if (is.null(arr_attrs[["units"]]) && !is.null(gis$unit) && nzchar(gis$unit)) {
    arr_attrs[["units"]] <- gis$unit
  }

  Variable(
    dims  = gis$dim_names,
    data  = array(x, dim = gis$dim),
    attrs = arr_attrs
  )
}


#' Read a lazy variable from backend on demand
#' @keywords internal
#' @noRd
backend_read_var <- function(be, var_name) {
  # Check cache first
  if (exists(var_name, envir = be$cache, inherits = FALSE)) {
    return(get(var_name, envir = be$cache, inherits = FALSE))
  }

  # Open connection, read, close
  ds_handle <- new(
    gdalraster_class("GDALMultiDimRaster"),
    be$dsn, TRUE, character(), FALSE
  )
  on.exit(ds_handle$close(), add = TRUE)

  v <- read_var_gdal(ds_handle, var_name)

  # Cache for next access
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
