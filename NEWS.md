# ndr (development)

## Lazy `sel()` → `collect()` pipeline

* `LazyDataArray` class — lazy representation of a variable that carries a
  selection spec rather than data. Created automatically when accessing
  variables from `open_dataset()` via `ds$var_name`.
* `sel()` and `isel()` on LazyDataArray accumulate selections without reading
  data. Chained `isel()` calls compose indices correctly.
* `collect()` materialises a LazyDataArray into a DataArray via a single
  GDAL `mdim_array_read()` hyperslab call. Only the selected slice is read.
* Auto-collect: arithmetic, reductions (`nd_mean()` etc.), and
  `as.data.frame()` on a LazyDataArray trigger `collect()` transparently.
* `collect()` on a DataArray is the identity — safe to call on anything.

## File backends

* `open_dataset()` — read multidimensional data sources into ndr Datasets.
  Supports any source GDAL's multidim API can read: NetCDF, HDF5, Zarr v2/v3,
  kerchunk-parquet virtual stores, VRT multidim. Works with local paths and
  GDAL virtual filesystems (`/vsicurl/`, `/vsis3/`, `/vsigs/`).
* **Lazy by default**: `open_dataset()` reads only coordinates and metadata.
  Data variables load on first `collect()`. Use `vars` to scope which
  variables are available.

## CF time decoding

* `cf_decode_time()` — decode CF convention time values ("days since ...",
  "hours since ...") to R Date or POSIXct objects.
* Three-tier time unit detection: GDAL dimension metadata (`coord_info$type`),
  array attributes (`.zattrs`), and `mdim_array_info()$unit`. Covers NetCDF,
  Zarr, and HDF5 sources where GDAL reports CF units differently.
* Non-standard calendars (360_day, noleap, all_leap) delegate to the CFtime
  package when available. Standard/gregorian calendars use base R arithmetic.
* CFtime added to Suggests.

## Operator dispatch

* Fixed S7 Ops registration: replaced broken `local()` loop with explicit
  top-level `method<-` calls for all arithmetic and comparison operators.
* Unary `-` and `+` supported via `class_missing`.
* LazyDataArray operators auto-collect before computing.
* Scalar × scalar broadcast edge case fixed.

## Ergonomics

* `names()` and tab-completion (`.DollarNames`) on Dataset objects — `ds$`
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
* `LazyDataArray` stores `.selection` (named list of index vectors) and
  `.backend` (dsn + var_name) for deferred reads.
* `selection_to_hyperslab()` translates R-order selections to GDAL C-order
  start/count vectors.
* `ds_dims()` and Dataset print include dimensions from lazy variable schemas.
* `is_regular()` detects regularly-spaced coordinate values to choose between
  ImplicitCoord and ExplicitCoord when reading from files.
* gdalraster added to Suggests.


# ndr 0.1.0

Initial release. Named-dimension arrays with coordinate-based indexing for R.

## Core classes (S7)

* `Variable` — N-dimensional array with named dimensions and attributes.
* `DataArray` — Variable with attached coordinates for label-based access.
* `Dataset` — collection of aligned Variables on shared dimensions. Extract variables with `$` or `[[`.
* `ImplicitCoord` — regular-grid coordinate defined by offset + step (zero memory, O(1) lookup). Slicing preserves implicitness when the subset is regular.
* `ExplicitCoord` — arbitrary coordinate values (numeric, Date, character, etc.).

## Operations

* Arithmetic and comparison operators (`+`, `*`, `>`, etc.) broadcast by dimension name, not position. Disjoint dimensions produce outer-product-style expansion.
* `sel()` — select by coordinate value (label-based, nearest-match for numeric).
* `isel()` — select by integer index (1-based).
* `nd_mean()`, `nd_sum()`, `nd_min()`, `nd_max()` — reduce along one or more named dimensions.

## Coercion

* `as.data.frame()` on DataArray produces long-format output (one row per cell) for use with ggplot2/dplyr.
* `as_variable()` convenience constructor from R arrays.

## Design notes

* Single dependency: S7.
* No file backends, lazy evaluation, or chunked compute — this is the foundation layer for those to build on.
