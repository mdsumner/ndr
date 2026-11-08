# ndr (development)

* The GDAL engine reads GeoZarr (`spatial:transform`, `proj:code`, ...)
  and CF grid mappings as the zaro engine does, so both engines give the
  same `AffineIndex` (identical transform, CRS and shape on EOPF
  Sentinel-2 L2A `measurements/reflectance/r10m`), and with a GeoZarr
  transform the x/y coordinate arrays are not read (that open 4.3 s ->
  3.3 s). GDAL's own route (CRS plus HORIZONTAL_X/Y dimensions) stays for
  formats that declare them, such as netCDF. The georeferencing helpers
  work on a small view either engine builds.
* Housekeeping: design notes move to `design/` (out of the package build,
  which clears the check NOTE), with the gdalraster-era roadmap and notes
  under `design/history/`; NEWS no longer describes the removed
  `LazyDataArray`; Suggests `zaro (>= 0.0.1.9035)`.

* The GDAL engine reads string arrays with GDAL7 from rgdal-dev/GDAL7#15
  (`read_mdarray()` returns character, `data_type_class`): string
  coordinates load at open, and small string data variables are read at
  open under the same `ndr.eager_string_max` limit as the zaro engine. On
  EOPF Sentinel-2 L2A `conditions/geometry` both engines now open the same
  dataset with identical values. With an older GDAL7 string arrays are
  skipped with a message, as before; compound arrays are always skipped.

* `open_dataset()` opens 1D arrays along another dimension as data
  variables (they were skipped): EOPF Sentinel-2 L2A `conditions/geometry`
  now has `mean_sun_angles` on `angle`. CF grid-mapping variables (named
  by a `grid_mapping` attribute, simple or extended form) are still left
  out.
* String variables, with the zaro engine and zaro >= 0.0.1.9034
  (hypertidy/zaro#27): string coordinates load at open (EOPF `angle`,
  `band`, `detector`), and string data variables are read at open when
  they have at most `getOption("ndr.eager_string_max", 1e6)` elements
  (larger ones are skipped with a message). The GDAL engine can't read
  string arrays yet (GDAL7 reads numeric arrays): they are skipped with a
  message instead of failing the open.

* The zaro engine reads all coordinate arrays in one pooled fetch
  (`zaro::zaro_read_arrays()`, hypertidy/zaro#21), so opening costs about one
  round trip, not one per coordinate, on HTTP stores. With an older zaro it
  reads them one at a time, as before.

* The zaro engine builds an `AffineIndex` for georeferenced grids. With
  GeoZarr attributes (`spatial:transform` in rasterio order,
  `spatial:dimensions`, `spatial:registration`, CRS from `proj:code` /
  `proj:wkt2` / `proj:projjson` or the grid mapping) the transform defines
  the grid and the x/y coordinate arrays are not read: an EOPF Sentinel-2
  group opens with one request (2.8 s, was 5.9 s), and the transform's
  pixel centres are identical to the stored coordinates. Otherwise a CF
  `grid_mapping` with `crs_wkt` and two regular coordinates gives one, as
  the GDAL engine does from GDAL's CRS.

* The online zaro tests skip without blosc (the public stores use it):
  under `R CMD check` only declared packages are visible.

* `open_dataset()` gains `group =` to open one group of a nested store,
  such as an EOPF Sentinel-2 product (sharded GeoZarr) whose bands are at
  `measurements/reflectance/r10m`. A group can also be named in the path
  after `.zarr/` (`".../product.zarr/measurements/reflectance/r10m"`),
  which is split into the store and the group so that the store's root
  consolidated metadata is used. The group's attributes become the
  Dataset's. Both engines: the zaro engine needs zaro with
  `zaro_dataset(group =)` (hypertidy/zaro#20); the GDAL engine walks
  GDAL7's groups.

* `byterefs://` sources (zaro's name for byterefs stores, alongside
  `mdimrefs://`) pick the zaro engine by default.

* `open_dataset()` picks the zaro engine by default for local and
  `http(s)://` paths ending in `.zarr` (when zaro is installed; else GDAL).
  `ZARR:` connection strings and `/vsi` paths still go to GDAL.

## zaro engine acceptance

* `inst/examples/zaro-engine-parity.R` compares `engine = "zaro"` with the
  GDAL engine on AODN sea level (Zarr V2, S3), BRAN2023 temp (VirtualiZarr
  Parquet, NCI THREDDS) and CMIP6 GFDL-ESM4 tas (Zarr V2, GCS), and
  reproduces the BRAN2023 30-point track from zaro's track-extract example.
  Values are identical wherever both engines have one. BRAN2023 land cells
  are `NA` with zaro and -10.0078 with GDAL, which does not mask the
  `_FillValue` there.

* `tests/testthat/test-open-zaro-remote.R` runs the cheap parts (metadata,
  coordinates, one point) when online.

## zaro engine for `open_dataset()`

* `open_dataset(engine = "zaro")` reads Zarr V2/V3 and virtual Zarr
  (VirtualiZarr Parquet, kerchunk, mdim-refs) through the zaro package, with
  no GDAL in the path. It is chosen by default for zaro objects and for
  `s3://`, `gs://`, `virtualizarr://`, `mdimrefs://` and `reference+*://`
  sources; `...` goes to `zaro::zaro()` (e.g. `anonymous`, `region`).
  Variables are lazy altarr arrays, CF-decoded per chunk; the packing
  attributes are kept in the Variable's `encoding`.

* Coordinate building and `vars =` scoping are shared between the GDAL and
  zaro engines.

## Indexes

* Coordinates are now indexes, after xarray's custom Index API. `Index` is
  an abstract S7 class with generics `index_dims()`, `index_sizes()`,
  `index_coords()`, `index_sel()`, `index_isel()`, `index_drop()` and
  `index_equals()`. `ImplicitCoord` and `ExplicitCoord` implement it; any
  package can add a new kind of index. `sel()`, `isel()`, reductions and
  `as.data.frame()` go through the contract.
* Arithmetic now checks alignment: when both operands have an index on a
  shared dim and the indexes differ, it is an error. Set
  `options(ndr.join = "override")` for the old "take the left coords"
  behaviour.
* New `AffineIndex`: one index for an x/y grid from a GDAL geotransform
  (rotation allowed) plus its CRS. Regular subsets stay affine; irregular
  subsets of north-up grids become 1D coords. `as_geotransform()`,
  `set_affine_index()`.
* New `open_raster()` opens one band of a classic GDAL raster as a lazy
  DataArray with an `AffineIndex`. `open_dataset()` builds an
  `AffineIndex` when a variable has a CRS and regular HORIZONTAL_X /
  HORIZONTAL_Y coordinates.

## GDAL7 backend

* `open_dataset()` now reads through GDAL7 (`rgdal-dev/GDAL7`, GDAL >= 3.10)
  instead of the multidim branch of gdalraster. Lazy variables are made by
  GDAL7's `as_altarr()`, so each batch of chunks is one advised GDAL read
  (decoded on GDAL's threads for Zarr).
* CF `scale_factor`/`add_offset` are applied by ndr in a lazy view, since
  GDAL7 applies only nodata. Until GDAL7's `read_mdarray()` gains a `type`,
  integer arrays read as double.
* `Remotes` is now `rgdal-dev/GDAL7, hypertidy/altarr` (a duplicated
  `Remotes` field in DESCRIPTION is fixed).

## Lazy data via altarr

* A Variable's `data` can be a lazy chunked array from the altarr package
  (`hypertidy/altarr`, in Suggests): a plain array with a `dim` attribute
  that reads its chunks only when asked. See `?lazy-data`.
* `ds$var_name` on a Dataset from `open_dataset()` now returns an ordinary
  DataArray whose data is such an array, chunked like the file (GDAL's block
  size). `LazyDataArray` is gone: lazy data is a property of the data, not a
  separate class, so every method works on it.
* `isel()` and `sel()` on lazy data read nothing: they return a lazy view of
  the selection that reads only the source chunks it needs, with one planned
  read per batch of chunks (no element-by-element `[` reads).
* `nd_mean()`, `nd_sum()`, `nd_min()` and `nd_max()` on lazy data stream
  chunk-aligned blocks (one planned read each) instead of materialising the
  array with `apply()`; block size is `getOption("ndr.block_values")`.
* `collect()` now reads lazy data into memory for Variable, DataArray and
  Dataset, and is the identity for in-memory data. Arithmetic, `as.array()`
  and `as.data.frame()` read lazy data as before.
* Lazy Variables and their selections save with `saveRDS()` as recipes (the
  dsn, variable name and selection), not values.

## File backends

* `open_dataset()` - read multidimensional data sources into ndr Datasets.
  Supports any source GDAL's multidim API can read: NetCDF, HDF5, Zarr v2/v3,
  kerchunk-parquet virtual stores, VRT multidim. Works with local paths and
  GDAL virtual filesystems (`/vsicurl/`, `/vsis3/`, `/vsigs/`).
* **Lazy by default**: `open_dataset()` reads only coordinates and metadata.
  Data variables are lazy arrays (see "Lazy data via altarr"); values are
  read when used, so 12TB+ datasets open without reading any array data.
  Use `vars` to limit which variables are available.

## CF Time decoding 

* `cf_decode_time()` - decode CF convention time values ("days since ...",
  "hours since ...") to R Date or POSIXct objects.
* Time units are found in the array's unit (GDAL gives CF units there) or
  its `units` attribute. Covers NetCDF, Zarr, and HDF5 sources where GDAL
  reports CF units differently.
* Non-standard calendars (360_day, noleap, all_leap) delegate to the CFtime
  package when available. Standard/gregorian calendars use base R arithmetic.
* CFtime added to Suggests.

## Operator dispatch

* Fixed S7 Ops registration: replaced broken `local()` loop with explicit
  top-level `method<-` calls for all arithmetic and comparison operators.
* Unary `-` and `+` supported via `class_missing`.
* Operators on lazy data read it before computing.
* Scalar x scalar broadcast edge case fixed.

## Ergonomics

* `names()` and tab-completion (`.DollarNames`) on Dataset objects - `ds$`
  now shows available variable names in RStudio.
* `coord_lookup()` for ExplicitCoord handles Date and POSIXt values with
  nearest-neighbor matching (binary search on sorted coords, linear scan
  otherwise).
* ImplicitCoord gives a clear error when Date values are passed to an
  undecoded numeric time coordinate.
* `.onLoad` calls `S7::methods_register()` for reliable method discovery.

## Internal

* Dataset gains `.backend` property (default NULL) for lazy reading. Existing
  code that constructs Dataset objects directly is unaffected.
* `ds_dims()` and Dataset print include dimensions from lazy variable schemas.
* `is_regular()` detects regularly-spaced coordinate values to choose between
  ImplicitCoord and ExplicitCoord when reading from files.
* gdalraster added to Suggests (replaced by GDAL7 since).


# ndr 0.1.0

Initial release. Named-dimension arrays with coordinate-based indexing for R.

## Core classes (S7)

* `Variable` - N-dimensional array with named dimensions and attributes.
* `DataArray` - Variable with attached coordinates for label-based access.
* `Dataset` - collection of aligned Variables on shared dimensions. Extract variables with `$` or `[[`.
* `ImplicitCoord` - regular-grid coordinate defined by offset + step (zero memory, O(1) lookup). Slicing preserves implicitness when the subset is regular.
* `ExplicitCoord` - arbitrary coordinate values (numeric, Date, character, etc.).

## Operations

* Arithmetic and comparison operators (`+`, `*`, `>`, etc.) broadcast by dimension name, not position. Disjoint dimensions produce outer-product-style expansion.
* `sel()` - select by coordinate value (label-based, nearest-match for numeric).
* `isel()` - select by integer index (1-based).
* `nd_mean()`, `nd_sum()`, `nd_min()`, `nd_max()` - reduce along one or more named dimensions.

## Coercion

* `as.data.frame()` on DataArray produces long-format output (one row per cell) for use with ggplot2/dplyr.
* `as_variable()` convenience constructor from R arrays.

## Design notes

* Single dependency: S7.
* No file backends, lazy evaluation, or chunked compute - this is the foundation layer for those to build on.
