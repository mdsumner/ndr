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
#' @param vars Character vector of variable names to read. Default `NULL` reads
#'   all data variables. Coordinate arrays are always read.
#' @param ... Reserved for future use (e.g. `lazy = TRUE`).
#'
#' @return A [Dataset] with data variables, coordinates, and global attributes.
#'
#' @details
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
#' # Local NetCDF
#' ds <- open_dataset("sst.mnmean.nc")
#' ds$sst
#'
#' # Remote kerchunk-parquet via GDAL virtual filesystem
#' dsn <- 'ZARR:"/vsicurl/https://raw.githubusercontent.com/mdsumner/virtualized/refs/heads/main/remote/ocean_temp_2023.parq"'
#' ds <- open_dataset(dsn, vars = "temp")
#' ds$temp |> sel(Time = as.Date("2010-06-15"), st_ocean = 2.5)
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

  for (nm in array_names) {
    arr <- ds$openArrayFromFullname(paste0("/", nm), character())
    info <- gdalraster_fn("mdim_array_info")(arr)

    ndims <- length(info$dim_names)
    if (ndims == 0L) next  # skip scalar arrays
    if (ndims == 1L && info$dim_names == nm) {
      coord_names <- c(coord_names, nm)
    } else if (ndims > 1L) {
      data_var_names <- c(data_var_names, nm)
    }
    # skip: 1D arrays that don't match their dim name (bounds, auxiliary)
  }

  # Apply var filter
  if (!is.null(vars)) {
    missing <- setdiff(vars, data_var_names)
    if (length(missing) > 0L) {
      stop(sprintf(
        "requested variable(s) not found: %s\navailable: %s",
        paste(missing, collapse = ", "),
        paste(data_var_names, collapse = ", ")
      ))
    }
    data_var_names <- vars
  }

  # --- Phase 2: build coordinates ---
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

  # --- Phase 3: read data variables (eager) ---
  data_vars <- list()
  for (nm in data_var_names) {
    arr <- ds$openArrayFromFullname(paste0("/", nm), character())
    x <- gdalraster_fn("mdim_array_read")(arr)
    gis <- attr(x, "gis")

    # Build attrs from array attributes
    arr_attrs <- list()
    attr_names <- gdalraster_fn("mdim_array_attr_names")(arr)
    for (a in attr_names) {
      arr_attrs[[a]] <- tryCatch(
        gdalraster_fn("mdim_array_attr")(arr, a),
        error = function(e) NULL
      )
    }
    # Add unit from info if not already in attrs
    if (is.null(arr_attrs[["units"]]) && !is.null(gis$unit) && nzchar(gis$unit)) {
      arr_attrs[["units"]] <- gis$unit
    }

    data_vars[[nm]] <- Variable(
      dims = gis$dim_names,
      data = array(x, dim = gis$dim),
      attrs = arr_attrs
    )
  }

  # --- Phase 4: global attributes ---
  global_attrs <- tryCatch({
    root <- ds$getRootGroup()
    gdalraster_fn("mdim_group_attrs")(root)
  }, error = function(e) list())

  Dataset(data_vars = data_vars, coords = coords, attrs = global_attrs)
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
