#' Open a dataset from a file or URL
#'
#' Read a multidimensional data source into an ndr Dataset, through one of
#' two engines:
#'
#' * `"gdal"`: any source GDAL's multidim API can read (NetCDF, HDF5, Zarr
#'   v2/v3, kerchunk-parquet virtual stores, VRT multidim), from local paths,
#'   `/vsicurl/`, `/vsis3/` and other GDAL virtual filesystems. Needs the
#'   GDAL7 package (`remotes::install_github("rgdal-dev/GDAL7")`,
#'   GDAL >= 3.10).
#' * `"zaro"`: Zarr V2/V3 and virtual Zarr (kerchunk, VirtualiZarr Parquet,
#'   mdim-refs) read by the zaro package
#'   (`remotes::install_github("hypertidy/zaro")`), with no GDAL in the
#'   path. Takes anything [zaro::zaro()] opens, such as `s3://` and `gs://`
#'   URIs, `virtualizarr://` and `byterefs://` (or `mdimrefs://`) sources, or a store or
#'   [zaro::zaro_dataset()] object.
#'
#' Reading data needs the altarr package
#' (`remotes::install_github("hypertidy/altarr")`).
#'
#' @param dsn Data source name. A file path, URL, or GDAL connection string
#'   (e.g. `'ZARR:"/vsicurl/https://example.com/store.parq"'`).
#' @param group Path of the group to open in a nested store, e.g.
#'   `"measurements/reflectance/r10m"`. Default `NULL`, the root group (or
#'   the group named in `dsn` after `.zarr/`, see Details).
#' @param vars Character vector of variable names to include. Default `NULL`
#'   includes all data variables. Use `character()` for schema + coords only.
#'   All variables are loaded lazily on first access via `$`.
#' @param ... Passed to [zaro::zaro()] for the `"zaro"` engine (e.g.
#'   `anonymous = TRUE`, `region =`); otherwise unused.
#' @param engine `"gdal"` or `"zaro"`. The default picks `"zaro"` for zaro
#'   objects and for `s3://`, `gs://`, `virtualizarr://`, `byterefs://`, `mdimrefs://` and
#'   `reference+json://`/`reference+parquet://` sources, and for local or
#'   `http(s)://` paths ending in `.zarr` (or with a group after it) when zaro
#'   is installed; else
#'   `"gdal"` (including `ZARR:` connection strings and `/vsi` paths).
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
#' as a coordinate. Every other array with one or more dimensions is a data
#' variable, including 1D arrays along another dimension (such as a mean
#' per band) and bounds arrays (e.g. `time_bnds`). Scalar arrays are
#' skipped.
#'
#' Numeric and logical data variables are lazy. String data variables can't
#' be (lazy arrays are numeric), so they are read at open when they have at
#' most `getOption("ndr.eager_string_max", 1e6)` elements, and skipped with
#' a message when larger. String coordinates are read like any other. The
#' GDAL engine reads strings with GDAL7 from rgdal-dev/GDAL7#15; with an
#' older GDAL7 it skips string arrays with a message (string coordinates
#' too, leaving that dimension without coordinate values).
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
#' ## The zaro engine
#'
#' Consolidated metadata is one request for the whole store. Data
#' variables are CF-decoded as they are read: fill and missing values read
#' as `NA`, and `scale_factor`/`add_offset` are applied. Those attributes
#' move from the Variable's `attrs` to its `encoding`. Arrays without
#' dimension names get names from zaro (see [zaro::zaro_dataset()]).
#'
#' ## Georeferenced grids
#'
#' A georeferenced grid becomes one [AffineIndex], by the same rules in both
#' engines:
#'
#' * GeoZarr (`spatial:dimensions`, `spatial:transform`,
#'   `spatial:registration`, optional `spatial:shape`, on the group or the
#'   data variable): the transform defines the grid, so the x/y coordinate
#'   arrays are not read. The CRS comes from `proj:code`, `proj:wkt2` or
#'   `proj:projjson`, else the CF grid mapping.
#' * CF: a data variable whose `grid_mapping` variable has `crs_wkt` (or
#'   `spatial_ref`), and whose first two dims (R order) have regular
#'   coordinates.
#' * GDAL engine only, before the CF route: a data variable with a CRS from
#'   GDAL whose HORIZONTAL_X / HORIZONTAL_Y dimensions (as netCDF declares
#'   them) have regular coordinates.
#'
#' ## Groups
#'
#' A nested store (e.g. an EOPF Sentinel-2 product or other GeoZarr) keeps
#' its arrays in groups below the root. Open one group with `group =`, or
#' name it in the path: `"https://host/product.zarr/measurements/r10m"` is
#' read as `dsn = "https://host/product.zarr"`,
#' `group = "measurements/r10m"`, so the store's consolidated metadata (at
#' its root) is used. The group's attributes become the Dataset's. List a
#' store's groups with [zaro::zaro_groups()].
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
#'
#' # One group of a nested (sharded GeoZarr) store
#' eopf <- paste0("https://s3.explorer.eopf.copernicus.eu/esa-zarr-sentinel-explorer-fra/",
#'                "tests-output/sentinel-2-l2a/",
#'                "S2C_MSIL2A_20260120T084301_N0511_R064_T36UXA_20260120T122910.zarr")
#' r10m <- open_dataset(eopf, group = "measurements/reflectance/r10m")
#' r10m$b04 |> isel(x = 5001:6000, y = 5001:6000) |> collect()
#' }
#'
#' @export
open_dataset <- function(dsn, vars = NULL, ..., group = NULL, engine = NULL) {
  if (is.null(group)) {
    sp <- split_zarr_group(dsn)
    dsn <- sp$dsn
    group <- sp$group
  }
  group <- clean_group(group)
  if (is.null(engine)) engine <- guess_engine(dsn)
  engine <- match.arg(engine, c("gdal", "zaro"))
  if (engine == "zaro") {
    check_zaro()
    return(open_dataset_zaro(dsn, vars = vars, group = group, ...))
  }
  check_gdal7()
  open_dataset_gdal(dsn, vars = vars, group = group, ...)
}


#' A group named after `.zarr/` in a path: split it off
#'
#' `"https://h/p.zarr/a/b"` -> dsn `"https://h/p.zarr"`, group `"a/b"`.
#' GDAL connection strings and `/vsi` paths are left alone.
#' @keywords internal
#' @noRd
split_zarr_group <- function(dsn) {
  out <- list(dsn = dsn, group = "")
  if (!is.character(dsn) || length(dsn) != 1L || grepl("^(ZARR:|/vsi)", dsn)) {
    return(out)
  }
  m <- regmatches(dsn, regexec("^(.*?\\.zarr)/+(.*?)/*$", dsn, ignore.case = TRUE))[[1L]]
  if (length(m) == 3L && nzchar(m[3L])) out <- list(dsn = m[2L], group = m[3L])
  out
}

#' A group path without leading/trailing slashes ("" for the root)
#' @keywords internal
#' @noRd
clean_group <- function(group) {
  if (is.null(group)) return("")
  if (!is.character(group) || length(group) != 1L || is.na(group)) {
    stop("'group' must be a single path", call. = FALSE)
  }
  group <- gsub("^/+|/+$", "", group)
  if (group == ".") "" else group
}


#' Which engine reads this source by default
#' @keywords internal
#' @noRd
guess_engine <- function(dsn) {
  if (!is.character(dsn)) return("zaro")
  zaro_schemes <- "^(s3|gs|virtualizarr|byterefs|mdimrefs|reference\\+json|reference\\+parquet)://"
  if (grepl(zaro_schemes, dsn)) return("zaro")
  # a local or http(s) path ending in .zarr: zaro if installed (GDAL can
  # read these too, so fall back to it); GDAL connection strings and /vsi
  # paths stay with GDAL
  is_zarr_path <- grepl("\\.zarr/*$", dsn, ignore.case = TRUE) &&
    !grepl("^(ZARR:|/vsi)", dsn)
  if (is_zarr_path && requireNamespace("zaro", quietly = TRUE)) "zaro" else "gdal"
}


#' @keywords internal
#' @noRd
open_dataset_gdal <- function(dsn, vars = NULL, group = "", ...) {

  ds <- GDAL7::gdal_open(dsn, multidim = TRUE)
  on.exit(GDAL7::gdal_close(ds), add = TRUE)
  root <- GDAL7::get_root_group(ds)
  if (is.null(root)) {
    stop(sprintf("'%s' is not a multidimensional source", dsn), call. = FALSE)
  }
  for (g in strsplit(group, "/", fixed = TRUE)[[1L]]) {
    sub_group <- if (g %in% root@group_names) GDAL7::open_group(root, g)
    if (is.null(sub_group)) {
      stop(sprintf("group '%s' is not in '%s'", group, dsn), call. = FALSE)
    }
    root <- sub_group
  }

  array_names <- root@mdarray_names

  # --- Phase 1: classify arrays as coords or data vars ---
  coord_names <- character()
  data_var_names <- character()
  string_names <- character()
  unreadable <- character()
  arrays <- list()  # open arrays, reused below

  for (nm in array_names) {
    arr <- GDAL7::open_mdarray(root, nm)
    if (is.null(arr)) next
    arrays[[nm]] <- arr
    dim_names <- arr@dimensions$name

    ndims <- length(dim_names)
    if (ndims == 0L) next  # skip scalar arrays
    klass <- gdal_value_class(arr)
    if (klass == "numeric" || (klass == "string" && ndims == 1L && dim_names == nm)) {
      if (ndims == 1L && dim_names == nm) {
        coord_names <- c(coord_names, nm)
      } else {
        data_var_names <- c(data_var_names, nm)
      }
    } else if (klass == "string") {
      string_names <- c(string_names, nm)
    } else {
      unreadable <- c(unreadable, nm)
    }
  }
  gm_names <- grid_mapping_names(lapply(arrays, function(a) a@attributes))
  data_var_names <- setdiff(data_var_names, gm_names)
  string_names <- setdiff(string_names, gm_names)
  unreadable <- setdiff(unreadable, gm_names)
  if (length(unreadable)) {
    message("ndr: skipping array(s) the GDAL engine can't read: ",
            paste(unreadable, collapse = ", "),
            if (gdal7_reads_strings()) " (compound values)" else
              " (strings need a newer GDAL7; engine = \"zaro\" reads them)")
  }

  # --- Phase 2a: a GeoZarr transform defines the x/y grid, as in the zaro
  # engine; its coordinate arrays are then not read ---
  root_attrs <- tryCatch(root@attributes, error = function(e) list())
  gz <- gdal_geo_view(root_attrs, arrays)
  affine <- geozarr_affine(gz, data_var_names)
  if (!is.null(affine)) coord_names <- setdiff(coord_names, affine@dims)

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

    coords[[nm]] <- make_coord(nm, vals, time_units, arr_attrs[["calendar"]])
  }

  # --- Phase 2b: a georeferenced x/y pair becomes one AffineIndex ---
  # When a data variable has a CRS and its HORIZONTAL_X / HORIZONTAL_Y
  # dimensions both have regular coordinates, the grid and its CRS are
  # carried together (see ?AffineIndex).
  if (!is.null(affine)) {
    coords <- c(stats::setNames(list(affine), paste(affine@dims, collapse = ",")),
                coords)
  } else {
    coords <- affine_coords_from_arrays(coords, arrays[data_var_names])
    # Zarr gives GDAL no dimension types: then the CF grid mapping route,
    # as in the zaro engine
    if (!any(vapply(coords, function(cc) S7::S7_inherits(cc, AffineIndex), NA))) {
      coords <- cf_affine_coords(coords, gz, data_var_names)
    }
  }

  # --- Phase 3: determine scope; small string variables are read now ---
  in_scope <- scope_vars(vars, c(data_var_names, string_names))
  data_var_names <- intersect(data_var_names, in_scope)
  string_names <- eager_string_names(
    intersect(string_names, in_scope),
    lapply(arrays[intersect(string_names, in_scope)],
           function(a) a@dimensions$size))
  data_vars <- list()
  for (nm in string_names) {
    arr <- arrays[[nm]]
    x <- GDAL7::read_mdarray(arr)       # dim already in R order
    x <- array(as.vector(x), rev(as.integer(arr@dimensions$size)))
    a <- arr@attributes
    data_vars[[nm]] <- Variable(dims = rev(arr@dimensions$name), data = x,
                                attrs = a)
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
  global_attrs <- root_attrs

  # --- Build backend (if there are lazy vars) ---
  backend <- NULL
  if (length(schemas) > 0L) {
    backend <- list(
      dsn     = dsn,
      group   = group,
      schemas = schemas,
      cache   = new.env(parent = emptyenv())
    )
  }

  Dataset(
    data_vars = data_vars,
    coords    = coords,
    attrs     = global_attrs,
    .backend  = backend
  )
}


#' @keywords internal
#' @noRd
open_dataset_zaro <- function(dsn, vars = NULL, group = "", ...) {
  zd <- if (inherits(dsn, "zaro_dataset")) {
    if (nzchar(group)) {
      stop("'group' can't be given with a zaro_dataset; ",
           "use zaro::zaro_dataset(group = )", call. = FALSE)
    }
    dsn
  } else if (nzchar(group)) {
    if (!"group" %in% names(formals(zaro::zaro_dataset))) {
      stop("opening a group needs a newer zaro: ",
           "remotes::install_github(\"hypertidy/zaro\")", call. = FALSE)
    }
    zaro::zaro_dataset(dsn, group = group, verbose = FALSE, ...)
  } else {
    zaro::zaro_dataset(dsn, verbose = FALSE, ...)
  }

  # --- classify, by the same rule as the GDAL engine ---
  coord_names <- character()
  data_var_names <- character()
  string_names <- character()
  gm_names <- grid_mapping_names(lapply(zd$arrays, function(m) m@attributes))
  for (nm in names(zd$arrays)) {
    dims <- zd$dims[[nm]]
    dt <- zd$arrays[[nm]]@data_type
    if (length(dims) == 0L) next  # scalar
    if (nm %in% gm_names) next    # a CF grid mapping: CRS, not data
    if (length(dims) == 1L && dims == nm) {
      coord_names <- c(coord_names, nm)
    } else if (grepl("^(u?int|float|bool)", dt)) {
      data_var_names <- c(data_var_names, nm)
    } else if (is_string_dtype(dt)) {
      string_names <- c(string_names, nm)
    }
  }

  # --- a GeoZarr transform defines the x/y grid: no coordinate read ---
  gz <- geo_view(zd$attributes,
                 lapply(zd$arrays, function(m) m@attributes),
                 zd$shape, zd$dims)
  affine <- geozarr_affine(gz, data_var_names)
  if (!is.null(affine)) coord_names <- setdiff(coord_names, affine@dims)

  # --- scope, and which string variables are small enough to read now ---
  in_scope <- scope_vars(vars, c(data_var_names, string_names))
  data_var_names <- intersect(data_var_names, in_scope)
  string_names <- intersect(string_names, in_scope)
  string_names <- eager_string_names(string_names, zd$shape[string_names])

  # --- coordinates (always read; small) and small string variables, in
  # one fetch ---
  coords <- list()
  read_vals <- read_zaro_arrays(zd, c(coord_names, string_names))
  coord_vals <- lapply(read_vals[coord_names], as.vector)
  for (nm in coord_names) {
    vals <- coord_vals[[nm]]
    a <- zd$arrays[[nm]]@attributes
    u <- a[["units"]]
    time_units <- if (is.character(u) && length(u) == 1L &&
                      grepl("since", u, fixed = TRUE)) u else NULL
    coords[[nm]] <- make_coord(nm, vals, time_units, a[["calendar"]])
  }

  if (!is.null(affine)) {
    coords <- c(stats::setNames(list(affine), paste(affine@dims, collapse = ",")),
                coords)
  } else {
    coords <- cf_affine_coords(coords, gz, data_var_names)
  }

  # --- string variables: in memory, in R dimension order ---
  data_vars <- list()
  for (nm in string_names) {
    x <- read_vals[[nm]]
    d <- if (is.null(dim(x))) length(x) else dim(x)
    x <- array(as.vector(x), unname(d))
    if (!identical(zd$arrays[[nm]]@raw_meta[["order"]], "F")) {
      x <- aperm(x)
    }
    a <- zd$arrays[[nm]]@attributes
    a[["_ARRAY_DIMENSIONS"]] <- NULL
    data_vars[[nm]] <- Variable(dims = zd$dims[[nm]], data = x, attrs = a)
  }

  # --- schemas: CF packing attributes are encoding, data is decoded ---
  enc_keys <- c("scale_factor", "add_offset", "_FillValue", "missing_value")
  schemas <- list()
  for (nm in data_var_names) {
    a <- zd$arrays[[nm]]@attributes
    a[["_ARRAY_DIMENSIONS"]] <- NULL
    schemas[[nm]] <- list(
      dim_names = zd$dims[[nm]],
      dim_sizes = unname(zd$shape[[nm]]),
      attrs     = a[setdiff(names(a), enc_keys)],
      encoding  = a[intersect(names(a), enc_keys)]
    )
  }

  backend <- NULL
  if (length(schemas) > 0L) {
    backend <- list(
      engine  = "zaro",
      dsn     = zd,
      schemas = schemas,
      cache   = new.env(parent = emptyenv())
    )
  }

  Dataset(
    data_vars = data_vars,
    coords    = coords,
    attrs     = zd$attributes,
    .backend  = backend
  )
}


#' Names of CF grid-mapping variables referenced by arrays' attributes
#'
#' `grid_mapping` is a variable name, or the extended form
#' `"crs_a: x y crs_b: lat lon"`. These variables hold a CRS, not data.
#' @keywords internal
#' @noRd
grid_mapping_names <- function(attr_list) {
  gm <- unlist(lapply(attr_list, function(a) {
    g <- a[["grid_mapping"]]
    if (is.character(g) && length(g) == 1L) g
  }))
  if (!length(gm)) return(character())
  out <- unlist(lapply(gm, function(g) {
    if (grepl(":", g, fixed = TRUE)) {
      regmatches(g, gregexpr("[^[:space:]:]+(?=:)", g, perl = TRUE))[[1L]]
    } else trimws(g)
  }))
  unique(out[nzchar(out)])
}


#' The string variables small enough to read at open
#'
#' At most `getOption("ndr.eager_string_max", 1e6)` elements; the rest are
#' named in a message and left out.
#' @param nms variable names; `shapes` a list of their sizes, in order.
#' @keywords internal
#' @noRd
eager_string_names <- function(nms, shapes) {
  if (!length(nms)) return(character())
  eager_max <- getOption("ndr.eager_string_max", 1e6)
  n_el <- vapply(shapes, function(s) prod(as.double(s)), 1)
  if (any(n_el > eager_max)) {
    message("ndr: skipping string variable(s) larger than ",
            "getOption(\"ndr.eager_string_max\") = ", format(eager_max), ": ",
            paste(nms[n_el > eager_max], collapse = ", "))
  }
  nms[n_el <= eager_max]
}

#' What kind of value a GDAL array holds: "numeric", "string" or "compound"
#'
#' GDAL7 >= the string-reading version has `data_type_class`; before that a
#' non-numeric array has an empty `data_type_name` and can't be read.
#' @keywords internal
#' @noRd
gdal_value_class <- function(arr) {
  if (gdal7_reads_strings()) return(arr@data_type_class)
  if (nzchar(arr@data_type_name)) "numeric" else "unreadable"
}

#' Does this GDAL7 read string arrays? (rgdal-dev/GDAL7#15)
#' @keywords internal
#' @noRd
gdal7_reads_strings <- function() {
  exists("GDAL7_mdarray_read_strings", envir = asNamespace("GDAL7"),
         inherits = FALSE)
}

#' Zarr string data types, as zaro names them (older zaro: "string")
#' @keywords internal
#' @noRd
is_string_dtype <- function(dt) {
  dt %in% c("fixed_length_utf32", "null_terminated_bytes", "string")
}


#' Values of whole arrays read at open (coordinates, small strings), together
#'
#' One `zaro::zaro_read_arrays()` call, so the arrays' chunks share one
#' fetch (concurrent HTTP requests): opening costs about one round trip, not
#' one per array. Older zaro: one `zaro_read()` per array. Arrays keep
#' their dims, in Zarr order.
#' @keywords internal
#' @noRd
read_zaro_arrays <- function(zd, names) {
  if (!length(names)) return(list())
  if (exists("zaro_read_arrays", envir = asNamespace("zaro"), inherits = FALSE)) {
    vals <- zaro::zaro_read_arrays(zd, names, verbose = FALSE)
    attr(vals, "stats") <- NULL
    return(vals)
  }
  stats::setNames(lapply(names, function(nm) {
    path <- if (is.null(zd$paths)) nm else zd$paths[[nm]]
    zaro::zaro_read(zd$store, path, meta = zd$arrays[[nm]], verbose = FALSE)
  }), names)
}


#' What the GeoZarr and CF georeferencing helpers need, from either engine
#'
#' @param group_attrs the group's attributes.
#' @param var_attrs named list: each array's attributes.
#' @param shape named list: each array's sizes, named by dimension, in R
#'   order.
#' @param dims named list: each array's dimension names, in R order.
#' @keywords internal
#' @noRd
geo_view <- function(group_attrs, var_attrs, shape, dims) {
  list(attributes = if (is.null(group_attrs)) list() else group_attrs,
       var_attrs = var_attrs, shape = shape, dims = dims)
}

#' A geo view from the GDAL engine's open arrays
#' @keywords internal
#' @noRd
gdal_geo_view <- function(group_attrs, arrays) {
  dims <- lapply(arrays, function(a) rev(a@dimensions$name))
  shape <- lapply(arrays, function(a) {
    stats::setNames(rev(as.integer(a@dimensions$size)), rev(a@dimensions$name))
  })
  geo_view(group_attrs, lapply(arrays, function(a) a@attributes), shape, dims)
}


#' An AffineIndex from GeoZarr attributes, or NULL
#'
#' The spatial convention (`spatial:dimensions`, `spatial:transform` in
#' rasterio/Affine order `[a, b, c, d, e, f]`, `spatial:registration`,
#' `spatial:shape`) with a CRS from the proj convention (`proj:code`,
#' `proj:wkt2`, `proj:projjson`) or the CF grid mapping. Looked for on the
#' group, then on the data variables. The transform is the grid's
#' definition, so the x/y coordinate arrays are not read.
#' @keywords internal
#' @noRd
geozarr_affine <- function(gz, data_var_names) {
  cands <- c(list(gz$attributes), unname(gz$var_attrs[data_var_names]))
  for (a in cands) {
    ix <- tryCatch(affine_from_geozarr(a, gz, data_var_names),
                   error = function(e) NULL)
    if (!is.null(ix)) return(ix)
  }
  NULL
}

#' @keywords internal
#' @noRd
affine_from_geozarr <- function(a, gz, data_var_names) {
  tr <- suppressWarnings(as.numeric(unlist(a[["spatial:transform"]])))
  sd <- as.character(unlist(a[["spatial:dimensions"]]))
  if (length(tr) != 6L || anyNA(tr) || length(sd) != 2L) return(NULL)
  dims <- rev(sd)                       # (rows, cols) -> (cols, rows)
  # the dims must be a data variable's, with sizes that agree
  sizes <- NULL
  for (nm in data_var_names) {
    s <- gz$shape[[nm]]
    if (all(dims %in% names(s))) { sizes <- unname(s[dims]); break }
  }
  if (is.null(sizes)) return(NULL)
  ss <- suppressWarnings(as.integer(unlist(a[["spatial:shape"]])))
  if (length(ss) == 2L && !anyNA(ss) && !all(rev(ss) == sizes)) return(NULL)
  # rasterio (a, b, c, d, e, f) -> GDAL (c, a, b, f, d, e)
  gt <- c(tr[3L], tr[1L], tr[2L], tr[6L], tr[4L], tr[5L])
  if (identical(a[["spatial:registration"]], "node")) {
    # the transform locates pixel centres: move it to the corner
    gt[1L] <- gt[1L] - 0.5 * (gt[2L] + gt[3L])
    gt[4L] <- gt[4L] - 0.5 * (gt[5L] + gt[6L])
  }
  AffineIndex(dims = dims, shape = as.integer(sizes), transform = gt,
              crs = geozarr_crs(a, gz, data_var_names))
}

#' CRS from the proj convention, else the CF grid mapping
#' @keywords internal
#' @noRd
geozarr_crs <- function(a, gz, data_var_names) {
  for (k in c("proj:code", "proj:wkt2")) {
    v <- a[[k]]
    if (is.character(v) && length(v) == 1L && nzchar(v)) return(v)
  }
  pj <- a[["proj:projjson"]]
  if (!is.null(pj) && requireNamespace("jsonlite", quietly = TRUE)) {
    return(as.character(jsonlite::toJSON(pj, auto_unbox = TRUE, digits = NA)))
  }
  gm <- c(a[["grid_mapping"]],
          unlist(lapply(gz$var_attrs[data_var_names],
                        function(va) va[["grid_mapping"]])))
  grid_mapping_crs(gz, gm)
}

#' CRS (WKT) of the first CF grid mapping variable that has one
#' @keywords internal
#' @noRd
grid_mapping_crs <- function(gz, gm) {
  for (g in unique(as.character(gm))) {
    ga <- gz$var_attrs[[g]]
    if (is.null(ga)) next
    for (k in c("crs_wkt", "spatial_ref")) {
      v <- ga[[k]]
      if (is.character(v) && length(v) == 1L && nzchar(v)) return(v)
    }
  }
  character()
}

#' CF route: two regular coordinates of a data variable with a grid
#' mapping CRS become one AffineIndex
#'
#' The x and y dims are a data variable's first two in R order (its last
#' two, (y, x), in Zarr's C order), as CF and xarray write them.
#' @keywords internal
#' @noRd
cf_affine_coords <- function(coords, gz, data_var_names) {
  for (nm in data_var_names) {
    a <- gz$var_attrs[[nm]]
    crs <- grid_mapping_crs(gz, a[["grid_mapping"]])
    if (!length(crs)) next
    d <- gz$dims[[nm]]
    if (length(d) < 2L) next
    xd <- d[1L]; yd <- d[2L]
    if (is.null(coords[[xd]]) || is.null(coords[[yd]])) next
    ix <- affine_from_coords(coords[[xd]], coords[[yd]], crs)
    if (is.null(ix)) next
    coords[c(xd, yd)] <- NULL
    return(c(stats::setNames(list(ix), paste(c(xd, yd), collapse = ",")), coords))
  }
  coords
}


#' A coordinate from its values: regular numeric -> ImplicitCoord
#'
#' CF time units (`"<unit> since <origin>"`) decode the values first.
#' @keywords internal
#' @noRd
make_coord <- function(nm, vals, time_units = NULL, calendar = NULL) {
  if (!is.null(time_units)) {
    vals <- tryCatch(
      cf_decode_time(vals, time_units, calendar),
      error = function(e) vals  # fall back to raw numeric
    )
  }
  if (is.numeric(vals) && length(vals) >= 2L && is_regular(vals)) {
    ImplicitCoord(
      dimension = nm,
      n         = length(vals),
      offset    = vals[1L],
      step      = regular_step(vals)
    )
  } else {
    ExplicitCoord(dimension = nm, values = vals)
  }
}


#' Which data variables to expose
#'
#' `vars = NULL` all (lazy), `vars = "sst"` only those, `vars = character()`
#' none (schema + coords only).
#' @keywords internal
#' @noRd
scope_vars <- function(vars, data_var_names) {
  if (is.null(vars)) return(data_var_names)
  if (length(vars) == 0L) return(character())
  missing <- setdiff(vars, data_var_names)
  if (length(missing) > 0L) {
    stop(sprintf(
      "requested variable(s) not found: %s\navailable: %s",
      paste(missing, collapse = ", "),
      paste(data_var_names, collapse = ", ")
    ))
  }
  vars
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
  data <- if (identical(be$engine, "zaro")) {
    zaro_lazy_array(be$dsn, var_name)
  } else {
    gdal_lazy_array(be$dsn, var_name, be$group)
  }
  v <- Variable(
    dims     = schema$dim_names,
    data     = data,
    attrs    = schema$attrs,
    encoding = if (is.null(schema$encoding)) list() else schema$encoding,
    source   = lazy_source(be, var_name, schema, data)
  )
  assign(var_name, v, envir = be$cache)
  v
}


#' @keywords internal
#' @noRd
affine_coords_from_arrays <- function(coords, arrays) {
  for (arr in arrays) {
    crs <- tryCatch(arr@crs, error = function(e) NA_character_)
    if (length(crs) != 1L || is.na(crs) || !nzchar(crs)) next
    gd <- arr@dimensions
    if (is.null(gd$type)) next
    xd <- gd$name[gd$type %in% "HORIZONTAL_X"]
    yd <- gd$name[gd$type %in% "HORIZONTAL_Y"]
    if (length(xd) != 1L || length(yd) != 1L) next
    if (is.null(coords[[xd]]) || is.null(coords[[yd]])) next
    ix <- affine_from_coords(coords[[xd]], coords[[yd]], crs)
    if (is.null(ix)) next
    coords[c(xd, yd)] <- NULL
    return(c(stats::setNames(list(ix), paste(c(xd, yd), collapse = ",")), coords))
  }
  coords
}


#' Open a raster band as a lazy DataArray
#'
#' Opens one band of a classic (2D) GDAL raster, such as a GeoTIFF or COG,
#' as a DataArray with lazy altarr data and an [AffineIndex] built from the
#' dataset's geotransform and CRS. Nothing but metadata is read until
#' values are asked for.
#'
#' @param dsn Data source name (file path, URL, or GDAL DSN string)
#' @param band Band number (1-based)
#' @param dims Names for the column and row dimensions
#' @return A DataArray with dims `dims` (columns first, as GDAL7 returns
#'   them)
#'
#' @examples
#' \dontrun{
#' r <- open_raster("/vsicurl/https://example.com/dem.tif")
#' r  # coords: (x, y) affine ...
#' r |> sel(x = c(140, 150), y = c(-45, -40)) |> collect()
#' }
#' @export
open_raster <- function(dsn, band = 1L, dims = c("x", "y")) {
  check_gdal7()
  check_altarr()
  ds <- GDAL7::gdal_open(dsn)
  gt <- ds@geotransform
  if (is.null(gt)) gt <- c(0, 1, 0, 0, 0, 1)
  crs <- tryCatch(ds@crs, error = function(e) character())
  crs <- crs[!is.na(crs) & nzchar(crs)]
  b <- GDAL7::get_raster_band(ds, band)
  data <- GDAL7::as_altarr(b)
  dimnames(data) <- NULL
  ix <- AffineIndex(
    dims = dims, shape = as.integer(dim(data)),
    transform = unname(as.double(gt)), crs = as.character(crs)
  )
  DataArray(
    variable = Variable(dims = dims, data = data),
    coords = stats::setNames(list(ix), paste(dims, collapse = ",")),
    name = sprintf("band_%d", as.integer(band))
  )
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
check_zaro <- function() {
  if (!requireNamespace("zaro", quietly = TRUE)) {
    stop(
      "zaro package is required for engine = \"zaro\".\n",
      "Install with: remotes::install_github(\"hypertidy/zaro\")",
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
