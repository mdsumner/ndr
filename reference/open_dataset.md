# Open a dataset from a file or URL

Read a multidimensional data source into an ndr Dataset. Currently
supports any source that GDAL's multidim API can read: NetCDF, HDF5,
Zarr v2/v3, kerchunk-parquet virtual stores, and VRT multidim. Works
with local paths, `/vsicurl/`, `/vsis3/`, and other GDAL virtual
filesystems.

## Usage

``` r
open_dataset(dsn, vars = NULL, ...)
```

## Arguments

- dsn:

  Data source name. A file path, URL, or GDAL connection string (e.g.
  `'ZARR:"/vsicurl/https://example.com/store.parq"'`).

- vars:

  Character vector of variable names to include. Default `NULL` includes
  all data variables. Use
  [`character()`](https://rdrr.io/r/base/character.html) for schema +
  coords only. All variables are loaded lazily on first access via `$`.

- ...:

  Reserved for future use.

## Value

A [Dataset](https://mdsumner.github.io/ndr/reference/Dataset.md) with
coordinates, global attributes, and lazy data variables whose values are
read only when used.

## Details

Requires the GDAL7 package
(`remotes::install_github("rgdal-dev/GDAL7")`, GDAL \>= 3.10) and, for
reading data, the altarr package
(`remotes::install_github("hypertidy/altarr")`).

### Lazy loading

`open_dataset()` reads only coordinates and metadata. Accessing a data
variable via `ds$var_name` returns a
[DataArray](https://mdsumner.github.io/ndr/reference/DataArray.md) whose
data is a lazy chunked array (see
[lazy-data](https://mdsumner.github.io/ndr/reference/lazy-data.md)):
still no array data is read.
[`sel()`](https://mdsumner.github.io/ndr/reference/indexing.md) and
[`isel()`](https://mdsumner.github.io/ndr/reference/indexing.md) stay
lazy, reductions stream the array chunk by chunk, and arithmetic or
[`collect()`](https://mdsumner.github.io/ndr/reference/collect.md) read
the selected values. This allows opening large datasets (e.g. 12TB
BRAN2023) without reading any array data. Use `vars` to limit which
variables are available. Lazy reads need the altarr package
(`remotes::install_github("hypertidy/altarr")`).

### Variable classification

Arrays are classified as coordinates or data variables based on CF
conventions: a 1D array whose name matches its dimension name is treated
as a coordinate. All other arrays with \>1 dimension are data variables.
Bounds arrays (e.g. `time_bnds`) and scalar arrays are skipped.

### Coordinate types

Regular spatial grids (equal spacing within floating-point tolerance)
are stored as
[ImplicitCoord](https://mdsumner.github.io/ndr/reference/ImplicitCoord.md)
(offset + step, no data allocation). Irregular grids and time
coordinates are stored as
[ExplicitCoord](https://mdsumner.github.io/ndr/reference/ExplicitCoord.md).

### CF time decoding

Time dimensions (GDAL type "TEMPORAL") are automatically decoded from
their CF units (e.g. "days since 1800-01-01") to R Date or POSIXct
values using
[`cf_decode_time()`](https://mdsumner.github.io/ndr/reference/cf_decode_time.md).

### Dimension ordering

Arrays are stored in R's column-major (Fortran) order, matching GDAL7's
`read_mdarray()` `$gis$dim` convention. Dimension names follow the same
order. For a NetCDF variable with dimensions (time, lat, lon), the R
array has `dim = c(nlon, nlat, ntime)` and
`dims = c("lon", "lat", "time")`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Open lazily - no data read yet
ds <- open_dataset("sst.mnmean.nc")
ds  # shows variables with [not loaded]

# Selecting is lazy too; collect() reads just the selected values
ds$sst |> sel(time = as.Date("2020-06-15"), lat = c(-60, -30)) |> collect()

# Reductions stream the array chunk by chunk
ds$sst |> sel(lat = c(-60, -30)) |> nd_mean("time", na.rm = TRUE)

# Scope to specific variables (still lazy)
ds <- open_dataset("sst.mnmean.nc", vars = "sst")

# Remote kerchunk-parquet - only sst schema, 12TB never touched
dsn <- 'ZARR:"/vsicurl/https://example.com/store.parq"'
ds <- open_dataset(dsn, vars = "temp")
ds$temp  # a lazy DataArray: nothing read yet
} # }
```
