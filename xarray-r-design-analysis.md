# Toward an R xarray: Core Abstractions, Ecosystem Analysis, and Design Sketch

## 1. What xarray Actually Is (Distilled)

xarray succeeds because it solves a very specific problem elegantly: it puts **names on dimensions** and **coordinates on axes**, then makes every operation respect those names. That's it. Everything else flows from that.

### The Data Model Stack

xarray has four public data structures, built in layers:

1. **Variable** — the hidden workhorse. A named-dimension array: `(dims, data, attrs, encoding)`. The `data` slot can hold numpy, dask, or any "duck array" that quacks right. Variable knows its dimension names and can broadcast, transpose, and reduce by name rather than by axis index. It does NOT know about coordinates.

2. **DataArray** — Variable + coordinate context. Wraps one Variable (the "data variable") plus a dict of additional Variables (coordinates), each of which must have dims that are a subset of the data variable's dims. When you do `da.sel(time='2020-01')`, the coordinate Variables are what make that work.

3. **Dataset** — a dict of aligned DataArrays on a shared set of dimensions. Internally it's a single `_variables` dict (both data vars and coords) plus a `_coord_names` set. The constraint is alignment: all variables sharing a dimension must agree on its length.

4. **DataTree** — nested Datasets in a hierarchy, mapping onto HDF5/Zarr group structures. Added in 2024 after a multi-year NASA-funded effort.

### The "Couldn't Do Without" Features

From practical xarray usage, these are the irreducible core capabilities:

**Named dimensions + broadcasting by name.** This is the single most transformative feature. `temperature * land_mask` just works even when their axes are in different orders, because xarray aligns by dimension name, not position. In R, we'd say this is like having `merge()` semantics built into arithmetic.

**Coordinate-based indexing (`.sel()`, `.isel()`).** Two indexing modes: by label (`.sel(lat=slice(-30, 30))`) and by integer position (`.isel(time=0)`). Both return objects with correct coordinates attached. This is what makes xarray vastly more useful than raw numpy+dicts.

**Lazy loading / backend abstraction.** `open_dataset()` returns a Dataset where the Variables hold `LazilyIndexedArray` wrappers around backend-specific array readers. Data isn't touched until you slice or compute. The backend system is pluggable (netCDF4, h5netcdf, scipy, zarr, GRIB via cfgrib).

**Multi-file aggregation (`open_mfdataset()`).** Scans many files, reads their coordinate metadata, and stitches them into one logical Dataset. Under the hood this uses dask to represent the chunked-across-files array. This is the killer feature for anyone working with model output or satellite time series.

**Digestible print summary.** The repr shows dimensions, coordinates, data variables with shapes/dtypes, and attributes, without loading data. This sounds trivial but it's critical for interactive exploration of datasets that are potentially terabytes.

**Chunked computation via dask integration.** Pass `chunks={}` at open time and every Variable wraps a dask array instead of numpy. All xarray operations become lazy graph construction. `ds.mean('time').compute()` triggers execution. This is not a separate API — the same `.sel()`, `.mean()`, `+` work on both eager and lazy arrays.

**Alignment and broadcasting on merge/concat.** `xr.concat()`, `xr.merge()`, `xr.align()` handle the messy reality of combining data with overlapping but not identical coordinates. This includes outer/inner/exact join semantics.

**Encoding/decoding.** The `encoding` dict on Variables stores information about on-disk representation (scale_factor, add_offset, _FillValue, dtype, compression). On read, these are automatically applied. On write, they're preserved. This is what makes the netCDF CF conventions actually usable.

### The Late Arrival: Flexible Indexing (xarray ≥ 2025.6)

xarray's most significant recent addition is the flexible `Index` API, which finally decouples coordinate representation from pandas.Index. This matters enormously and should be a foundational design principle in an R equivalent, not a late refactor.

The key insight: for regular grids, storing coordinate arrays is wasteful and sometimes impossible. A 0.00028° global DEM has ~811 billion coordinate values — 7 TB just for the x/y coordinates. But those coordinates are fully described by an affine transform: `X(i) = origin + i * step`. xarray now supports this via:

- **`RangeIndex`**: Coordinates defined by start/stop/step. Unlike `pandas.RangeIndex`, supports floating point. Lookup is O(1) arithmetic, not hash-table search. Slicing preserves the RangeIndex — you get a new start/stop, not a materialized array.

- **`CoordinateTransformIndex`**: Generalizes RangeIndex to arbitrary functions `X(i) = f(i)`. Coordinates are computed on demand, never stored. The `rasterix.RasterIndex` uses this with an affine transform for geospatial rasters — exactly what GDAL's GeoTransform does.

- **`CRSIndex` (via xproj)**: Coordinates carry CRS metadata, and `xr.align()` raises an error on CRS mismatch rather than silently producing nonsense. This is coordinate metadata *about* coordinates.

- **`GeometryIndex` (via xvec)**: Indexes a dimension by vector geometries with R-tree spatial indexing for efficient bbox queries. This makes xarray a viable vector datacube container.

**Implicit coordinates for regular grids should be a day-one design choice.** The cell abstraction from raster/terra (and packages like vaster that distil it) already represents regular grids as `(dimension, extent)` and derives coordinates on demand. The proposed R design should treat `RangeIndex`-style implicit coordinates as the *default* for regular grids from the start, not as an extension added years later. The internal coordinate model should be: a coordinate is either (a) an explicit array of values, or (b) a transform function from integer index to coordinate value. Both expose the same `.sel()` interface.

The rasterix/xproj story also validates the approach of keeping CRS awareness as an optional layer on top of the core dimension model — exactly the separation proposed in Section 5.


## 2. The Ecosystem Around xarray

### Zarr

Zarr is a chunked, compressed, N-dimensional array store specification. Key properties: each chunk is an independent object (file or cloud blob), metadata is JSON, and the chunk grid is regular. Zarr V3 (now the current spec) adds codecs pipelines, sharding, and a cleaner group model. The R `zarr` package on CRAN (by R-CF) implements Zarr V3 natively. `pizzarr` (keller-mark) implements an older Zarr spec. `Rarr` (Bioconductor) handles Zarr V2 read/write with bundled blosc.

For an R xarray, Zarr is important as a *storage backend* but NOT as the core abstraction. xarray doesn't care whether the backing store is netCDF4, Zarr, GRIB, or a numpy array — the Variable abstraction papers over all of this.

### VirtualiZarr

VirtualiZarr solves the "I have 10,000 netCDF files and want to treat them as one Zarr store" problem. It creates `ManifestArray` objects that record byte-range references into existing files, then lets you use xarray's `concat`/`combine` to stitch them together. The result is persisted as either Kerchunk JSON/Parquet references or (preferably) an Icechunk store.

The key insight: you don't need to actually rewrite data into Zarr format. You just need a lookup table of "chunk (i,j,k) lives at bytes 4096-8192 of file X". This is essentially what GDAL's VRT does for rasters, generalized to N dimensions.

For an R equivalent, this maps closely to what you've already explored with VRT and GDAL's virtual filesystem. The R design doesn't need to replicate VirtualiZarr — it needs the *concept* of a virtual chunk manifest.

### NetCDF and the multi-file problem

The netCDF C library + ncdf4 R package handle single-file access well. The pain point is always multi-file: climate model output is split across time, CMIP archives are split across variables and experiments, satellite L2 products are one-file-per-overpass. xarray's `open_mfdataset` and VirtualiZarr's `open_virtual_mfdataset` both solve this by reading coordinate metadata from each file, then building a unified index.

tidync already does the single-file abstraction well — it parses the netCDF schema into tidy tables of dimensions, variables, and transforms, then lets you slice by dimension name. What it doesn't do is span multiple files or provide computation.


## 3. R Building Blocks Available Now

### R spatial and NetCDF tooling

- **tidync**: NetCDF schema introspection and dimension-based slicing. Good model for parsing file metadata into a tidy representation. Doesn't span files, doesn't compute.
- **vapour / gdalraster**: GDAL-level access to raster and vector data. Both provide windowed reads from any GDAL source; gdalraster also offers solid grid abstraction tools.
- **vaster**: Raster grid logic (dimensions, extents, cell arithmetic) without any actual data. Essentially a pure-geometry distillation of the cell abstraction model from raster (and now terra, which improved on some of raster's conventions). Conceptually close to what xarray's dimension/coordinate system does for spatial axes.
- **dsn**: Data source name handling.
- **GDAL's multidimensional API**: Already handles VRT (virtual datasets), `GDALMDArray`, and a vast codec/format library. The multidimensional API is essentially a C-level xarray-like model, accessible from R via vapour or gdalraster.

### From the tidyverse / r-lib ecosystem

- **dbplyr's lazy operations pattern**: This is the closest R idiom to xarray's lazy evaluation model. `tbl(con, "table") |> filter(...) |> mutate(...)` builds a query tree, and `collect()` materializes it. The `lazy_query` S3 classes describe operations as a DAG. An R xarray could use an identical pattern: build an operation graph on dimension-aware arrays, materialize on `collect()` or `compute()`.

- **dtplyr**: Same idea — `lazy_dt()` wraps a data.table, records dplyr verbs as a translation plan, and `collect()` triggers evaluation as optimized data.table code.

- **vctrs**: Provides a type system for vectors including size/type compatibility checking. The `vec_recycle_common()`, `vec_cast()`, and `vec_ptype2()` mechanics could inform how broadcasting rules work for named-dimension arrays.

- **rlang**: Quosures, data masking, tidy evaluation. Useful for building user-facing DSLs like `.sel(time = "2020")`.

- **R6 or S7**: Class systems. R6 gives mutable reference semantics (useful if you want xarray-style in-place metadata updates). S7 (the new formal OOP system) gives multiple dispatch and proper encapsulation with immutable-by-default semantics.

### Other R packages — prior attempts and adjacent work

- **tbl_cube / cubelyr (Hadley Wickham)**: The earliest serious attempt at an xarray-like structure in the tidyverse. `tbl_cube` stores data as named dimensions (vectors) + measures (arrays), and supports `filter()` (on dimensions), `select()` (on measures), and `summarise()` with `group_by()`. The design was clean and prescient — dimensions and measures are cleanly separated, the print method shows `D:` and `M:` annotations, and it even included the NASA spatio-temporal dataset as a built-in example. However, it was explicitly experimental, never supported laziness, had no IO backends, and was limited to dense arrays. Hadley extracted it from dplyr into the standalone cubelyr package around 2019, effectively putting it in maintenance mode, as the sf/stars ecosystem became the dominant R spatial stack. tidync's `hyper_tbl_cube()` function bridges into this format, which confirms the model's basic soundness. The lesson: the data structure was right, but without backends and lazy evaluation, it couldn't gain traction for real-world scientific data.

- **rray (Davis Vaughan, r-lib)**: A more focused attack on a specific piece of the problem — NumPy-style broadcasting in R. rray provides an `rray` class backed by xtensor (C++) that implements proper broadcasting rules: `matrix(1:6, 3, 2) + matrix(1)` works instead of erroring. It also prevents dimension dropping (a persistent R footgun where `x[1,]` silently drops from matrix to vector). Built on vctrs for type stability. Presented at useR! 2019. The package was architecturally sound but was abandoned before reaching 1.0 — likely because xtensor development slowed and the maintenance burden of a C++ backend was high relative to the niche user base. rray tackled broadcasting *by position* (like NumPy), not *by name* (like xarray). The proposed design needs both: broadcasting by name is the higher-level feature, but rray's work on making R's `Ops` group generics and `[` method behave consistently with broadcasting rules is directly relevant to how we'd implement `Ops.Variable`.

- **dibble (Mizuki Uchida)**: Active on CRAN (v0.3.1, 2025). A "dimensional tibble" — a data frame where columns are classified as dimensions or measures, and arithmetic on measures broadcasts by dimension name. This is the closest existing R package to xarray's core broadcasting semantics. `as_dibble(arr1) * as_dibble(arr2)` aligns by named dimensions and broadcasts, producing NAs where coordinates don't overlap (outer join). Built on vctrs and rlang. The clever move: dibble represents data cubes *as* data frames (long format) rather than as nested arrays, which means it inherits all of tibble's printing, subsetting, and dplyr compatibility for free. The tradeoff is that for large dense arrays, the long-format representation is much less memory efficient than actual arrays. dibble also has no concept of laziness, chunking, or file backends. But the broadcasting-by-name semantics and the clean dimension/measure separation are exactly right. dibble is worth studying carefully — it may be that for small in-memory cubes, dibble *is* the right answer, and the proposed design only needs to exist for the lazy/multi-file/large-data case. Alternatively, a `as_dibble()` / `as_dataset()` bridge would let users move between representations.

- **stars**: Attempts to be R's xarray. It represents spatiotemporal arrays with named dimensions, supports NetCDF/GDAL backends, lazy evaluation via `stars_proxy`, and has `st_apply()` for mapped operations. The intention is right but the execution suffers from trying to do too much (raster, vector datacubes, irregular arrays) with insufficient separation of concerns. The proxy mechanism is fragile, and the dimension model is entangled with sf's spatial semantics.

- **terra/raster**: Great for 2D+band rasters but fundamentally 2.5D — no true N-dimensional model, no named dimensions beyond x/y/band/time, no lazy multi-file aggregation.

- **ncmeta**: Metadata-only reading of netCDF files. Produces tidy data frames of dimensions, variables, attributes. A useful building block for the "scan files and build an index" step.

- **zarr (CRAN)**: Native Zarr V3 for R. Early but functional. Could serve as a storage backend.

- **ff**: Worth noting as prior art in R for memory-mapped, chunked array storage. ff (by Daniel Adler and others) provided file-backed vectors and arrays with transparent paging long before Zarr existed. It supported chunked read/write, hybrid RAM/disk storage, and virtual windows into larger-than-memory arrays. bigmemory is more widely known but ff had a more general and composable design — it wasn't limited to matrices and supported factor-like types and data.frame-like structures (`ffdf`). The package is mature but largely unmaintained now, and never connected to the NetCDF/HDF5 world. Still, ff's architecture of "small metadata object in R pointing at chunked data on disk with transparent access" is essentially the same pattern that Zarr formalizes.

- **matter (Bioconductor)**: A more recent take on the same problem, designed for mass spectrometry imaging data. matter provides `matter_vec`, `matter_mat`, and `matter_arr` classes that are file-backed with deferred reads — the R object is a lightweight descriptor, and data is read from disk only when accessed. It supports multiple file formats and custom data layouts. Like ff, it demonstrates that R *can* have lazy, chunked array access without dask — the challenge is connecting this to the scientific data format ecosystem (NetCDF, Zarr, HDF5) and to the dimension-aware operations that xarray provides on top.

  Both ff and matter represent a road not taken: R had the storage backend concepts early, but never built the dimension-naming, coordinate-indexing, broadcasting layer on top. The Zarr wave brought that layer (via xarray/dask) in Python but R's earlier efforts at the storage level went unused.

### Arrow as a foundational layer

If starting from scratch, Apache Arrow deserves serious consideration as the in-memory representation and encoding/decoding engine, rather than building those pieces bespoke. The argument is not that Arrow replaces xarray's dimension model — it doesn't — but that it solves the *layer below* that, which xarray currently delegates to numpy+dask in an ad hoc way.

**What Arrow brings:**

- **Columnar encoding with built-in compression.** Arrow supports dictionary encoding, run-length encoding, delta encoding, and buffer-level compression (zstd, lz4, snappy). NetCDF's CF conventions (scale_factor, add_offset, _FillValue, packed integer types) are essentially hand-rolled versions of what Arrow does natively and generically. Instead of writing custom decode logic per-backend, an Arrow-based design could: read raw bytes from NetCDF/Zarr/GDAL → construct an Arrow array with the appropriate type and encoding → let Arrow's compute kernels handle the decode. This collapses the encoding/decoding problem.

- **Zero-copy interop.** Arrow's C Data Interface and C Stream Interface allow zero-copy handoff between R, Python, and any other language with Arrow bindings. A Dataset object backed by Arrow arrays could be passed to Python xarray via reticulate without serialization. nanoarrow (zero R dependencies, ~1MB installed) provides the R bindings for this interface.

- **ChunkedArray.** Arrow's `ChunkedArray` is exactly the abstraction needed for a lazy multi-file Variable: a logical array composed of chunks that may live in different files or memory regions. Each chunk can be independently compressed and decoded. This is what dask provides for xarray in Python, but Arrow gives it as a data structure rather than a task scheduler.

- **Tensor type.** Arrow C++ has an experimental `Tensor` type with named dimensions and strides. It's not heavily used yet, but it's there — and it means the Arrow project recognizes the need for N-dimensional data, not just columnar tables.

- **Compute kernels.** Arrow ships hundreds of compute functions (arithmetic, comparisons, aggregations, casts, string ops) that work directly on Arrow arrays without materializing to R vectors. A `mutate()` or `summarise()` on a Dataset could in principle compile down to Arrow compute calls.

- **The R arrow package is mature.** The arrow R package (backed by Arrow C++) and nanoarrow (minimal C implementation) are both well-maintained and widely used. DuckDB already uses Arrow as its interchange format. Polars is Arrow-native.

**The architectural argument:** In the proposed three-package split (core / IO / compute), Arrow could serve as the universal in-memory chunk representation in the IO layer. When a backend reads a chunk from NetCDF, Zarr, or GDAL, it produces an Arrow array (with encoding metadata preserved). When the compute layer needs to evaluate an expression, it works on Arrow arrays using Arrow compute kernels. When the user calls `as.array()`, Arrow materializes to R's native array. This means:

1. Backend authors only need to produce Arrow arrays, not R arrays (simpler, more formats get encoded correctly)
2. Encoding/decoding (scale_factor, fill values, type promotion) happens once in Arrow's type system, not reimplemented per backend
3. Zero-copy handoff to Python/Julia/DuckDB for users who need to cross language boundaries
4. The chunking story is solved by Arrow's existing `ChunkedArray` rather than invented from scratch

The main caution: Arrow is columnar (1D), not N-dimensional. The dimension-naming, coordinate-indexing, and broadcasting layers would still need to be built in R — Arrow handles the bytes, not the semantics. There's also a weight consideration: the full arrow R package is heavy (~50MB), though nanoarrow is lightweight. The design should probably target nanoarrow for the core, with optional arrow integration for compute.

### Other infrastructure

- **future / mirai**: Async/parallel evaluation frameworks. Analogous to dask for triggering chunk-level parallel computation.


## 4. Design Sketch: An R xarray Alpha

### Philosophy

The goal is not to port xarray's 200k lines of Python. It's to identify the **minimal viable abstraction** that:

1. Gives you named dimensions and coordinate-based indexing on lazy multi-file data
2. Plays well with existing R tools (tidyverse verbs, GDAL, ncdf4)
3. Is a foundation others can build on (new backends, new compute engines)

### Core Data Structures

```
Variable(dims, data, attrs, encoding)
│
├── dims: character vector of dimension names, e.g. c("time", "lat", "lon")
├── data: the backing array — could be:
│   │   - R array (eager, in-memory)
│   │   - A "lazy array" promise (file reference + slice spec)
│   │   - A chunked array descriptor (list of lazy array promises)
│   └── encoding: list(dtype, scale_factor, add_offset, fill_value, ...)
│
Coordinate: either explicit or implicit
│
├── ExplicitCoord: a Variable holding materialized coordinate values
└── ImplicitCoord: a transform + dimension length (raster/terra cell abstraction)
    ├── dim: character(1)
    ├── length: integer(1)
    ├── transform: function(i) → values  (e.g. offset + i * step)
    ├── inverse: function(v) → i          (for .sel() lookups)
    └── crs: optional CRS metadata
    [Materializes to a Variable on demand, but .sel() uses inverse directly]
│
DataArray(variable, coords, name)
│
├── variable: one Variable (the data)
├── coords: named list of Coordinate objects (explicit or implicit)
└── name: character(1)
│
Dataset(data_vars, coords, attrs)
│
├── data_vars: named list of Variables
├── coords: named list of Coordinate objects
└── constraint: all variables sharing a dim agree on its length
```

The `ImplicitCoord` is the critical design decision that xarray got wrong initially and only fixed in 2025. For regular grids (which dominate geospatial data), coordinates are fully described by an affine transform or a simple `seq(from, by, length.out)`. Storing them as arrays wastes memory and creates false complexity. R already has this concept in the cell abstraction from raster/terra — the design should lean into it from day one.

`ImplicitCoord$transform` and `ImplicitCoord$inverse` are the pair that makes `.sel()` work without materialization: `sel(lat = -30)` calls `inverse(-30)` to get the integer index, then passes that to `isel()`. For `RangeIndex`-style coords, inverse is just `round((value - offset) / step)`. For explicit coords, `.sel()` falls back to binary search or hash lookup on the materialized values.

### The Lazy Array Backend

This is where the dbplyr pattern applies. Define a `LazyArray` class that records:

```r
LazyArray <- function(source, shape, dtype, chunks = NULL) {
  structure(
    list(
      source = source,       # file path, URL, GDAL dsn, zarr path, ...
      shape = shape,          # integer vector of dimension lengths
      dtype = dtype,          # "float32", "int16", etc.
      chunks = chunks,        # chunk shape (NULL = unchunked)
      ops = list()            # pending operations (slice, transpose, ...)
    ),
    class = "LazyArray"
  )
}
```

Operations like `[i, j, ]` or `slice(start, stop)` don't read data — they append to `ops`. Materialization happens on `compute()` / `collect()` / `as.array()`, which walks the ops list and issues the minimal read to the backend.

For single-file NetCDF, `compute()` translates the accumulated slices into an `ncdf4::ncvar_get()` call with start/count. For GDAL sources, it becomes a windowed raster read via vapour or gdalraster. For Zarr, it's chunk-level reads via the `zarr` package.

### Named-Dimension Broadcasting

The key arithmetic innovation. Implement a `Ops` group generic for Variable/DataArray that:

1. Finds the union of dimension names across operands
2. Checks that shared dimensions have the same length
3. Inserts size-1 dimensions where needed (broadcasting)
4. Dispatches to the underlying array operation

```r
# Conceptual example
temp  # Variable with dims c("time", "lat", "lon"), shape c(365, 180, 360)
mask  # Variable with dims c("lat", "lon"), shape c(180, 360)

temp * mask
# → union dims: c("time", "lat", "lon")
# → mask gets broadcast along "time"
# → result has dims c("time", "lat", "lon"), shape c(365, 180, 360)
```

This is the hardest part to get right but also the highest-value feature. The cell abstraction from raster/terra could inform the spatial dimension handling.

### Coordinate Indexing

Two methods on DataArray:

```r
sel(da, time = "2020-01", lat = c(-30, 30))   # label-based
isel(da, time = 1, lat = 60:120)               # integer-based
```

`sel()` looks up the requested labels in the coordinate Variables, converts to integer indices, then delegates to `isel()`. `isel()` appends a slice operation to the LazyArray.

### Multi-File Opening

```r
open_mfdataset(
  paths,                    # character vector of file paths
  concat_dim = "time",      # dimension to concatenate along
  engine = "ncdf4"          # or "gdal", "zarr"
)
```

Implementation:
1. For each file, read metadata only (dimensions, coordinate values, variable names) — use ncmeta, tidync's schema reader, or GDAL's metadata API
2. Build a "virtual" Dataset where each Variable's LazyArray records which file and which slice within that file contains its data
3. Coordinates along `concat_dim` are concatenated in memory (they're small)
4. Everything else stays lazy until `compute()`

### Print Method

This is critically important for usability. The print method should show:

```
<Dataset>
Dimensions:  time: 365, lat: 180, lon: 360
Coordinates:
  * time     (time)     datetime  2020-01-01 ... 2020-12-31
  * lat      (lat)      float64   -89.5 ... 89.5
  * lon      (lon)      float64   0.5 ... 359.5
Data variables:
    sst      (time, lat, lon)  float32  [lazy, 3 files]
    ice_frac (time, lat, lon)  float32  [lazy, 3 files]
Attributes:
    source:   NOAA OISST v2.1
```

No data loaded. Everything derived from metadata.

### What NOT to Build (Yet)

- **Dask-like parallel scheduler.** Use future/mirai for parallel chunk evaluation, but don't build a DAG scheduler. Serial chunk-by-chunk is fine for an alpha.
- **GroupBy.** Important but not essential for alpha. Can be added as a higher-level operation.
- **Full Zarr codec support.** Use the `zarr` CRAN package or GDAL's Zarr driver. Don't reimplement compression.
- **DataTree.** Hierarchical datasets are a v2 feature.
- **Bespoke CF decoding.** If Arrow is adopted as the chunk representation, CF encoding (scale_factor, add_offset, _FillValue) can be expressed as Arrow type casts and null-masking rather than hand-rolled decode functions. If not using Arrow, this can be a separate pass — ncmeta and tidync already handle some of it.
- **Plotting.** Leave this to existing tools (ggplot2, tmap, etc.) — just make sure `as.data.frame()` / `as_tibble()` / `as_dibble()` works well for feeding into them.

### Package Structure

A possible split:

- **ndr**: Core Variable / DataArray / Dataset classes, broadcasting, coordinate indexing, print methods. Zero heavy dependencies. This is the "vctrs of N-dimensional arrays." dibble's pure-R broadcasting logic could inform or be reused here.
- **ndio**: Backend readers — ncdf4, GDAL (via vapour/gdalraster), zarr. Provides the `open_dataset()` / `open_mfdataset()` entry points. Depends on ndr + backend packages. If Arrow is adopted, nanoarrow lives here as the universal chunk interchange format between backends and the core.
- **ndops**: Deferred operations, lazy evaluation, chunk-based compute. The dbplyr-like operation recording layer. Depends on ndr, optionally on future. If Arrow is adopted, arrow compute kernels could serve as the default evaluation engine.

The split keeps the core clean and testable. Someone who just wants named-dimension arrays on in-memory data can use ndr alone.


## 5. Comparison with Existing R Approaches

### The landscape of prior attempts

R has seen at least five serious attempts at something like xarray, each attacking a different piece of the problem. Understanding why each fell short is essential to avoiding the same fate.

| Package | Named dims | Broadcasting | Lazy/chunked | File backends | Active |
|---------|-----------|-------------|-------------|--------------|--------|
| tbl_cube/cubelyr | ✓ (D/M split) | ✗ | ✗ | ✗ | maintenance |
| rray | ✗ (by position) | ✓ (NumPy-style) | ✗ | ✗ | abandoned |
| dibble | ✓ (by name) | ✓ (by name) | ✗ | ✗ | active |
| stars | ✓ | partial | partial (proxy) | ✓ (GDAL, NetCDF) | active |
| proposed | ✓ (by name) | ✓ (by name) | ✓ (fundamental) | ✓ (pluggable) | — |

**tbl_cube** got the data model right (dimensions vs measures, clean separation) but had no path to real data. It was a data structure without an ecosystem. The proposed design inherits its conceptual clarity but adds lazy backends.

**rray** got broadcasting right at the mechanical level (C++ xtensor backend, proper Ops generics) but operated on positional axes, not named dimensions. It also suffered from the maintenance weight of a C++ dependency. The proposed design learns from rray that broadcasting *must* be in the Ops group generics to feel native, but does it by name rather than position. Depending on whether xtensor is still viable, rray's C++ broadcasting engine could potentially be reused for the inner loop of named-dimension broadcasting, after the name-alignment step resolves which axes correspond.

**dibble** is the most interesting current competitor. It implements named-dimension broadcasting in pure R, on CRAN, with a clean vctrs-based design. The key difference: dibble stores cubes as long-format data frames, which is brilliant for small cubes (free tibble integration) but catastrophic for the scientific data case. A 365 × 180 × 360 temperature field has 23.7 million cells — storing that as a 23.7M-row data frame with three dimension columns is ~10× the memory of the raw array. For in-memory cubes under ~1M cells, dibble may actually be the better choice. The proposed design targets the regime where data is too large, too chunked, or too file-bound for dibble's approach. An `as_dibble()` method on DataArray would make the two interoperable.

### stars in detail

stars tries to be everything — vector datacubes, raster datacubes, irregular grids, NetCDF, GDAL, curvilinear coordinates. This ambition leads to a complicated internal model where a `stars` object is a list of arrays with a `dimensions` attribute that encodes both the grid geometry and the coordinate semantics.

The proposed design differs in key ways:

- **Separation of dimension logic from spatial semantics.** A Variable knows it has a dimension called "lat" with 180 elements. It doesn't know or care that this represents latitude. Spatial awareness is a higher-level concern.
- **Explicit lazy/eager distinction.** stars' `stars_proxy` is a bolted-on afterthought. In the proposed design, laziness is fundamental — every Variable can be backed by a LazyArray.
- **Backend pluggability.** stars hardcodes its backends (GDAL, NetCDF). The proposed design uses a generic backend interface that new engines can implement.
- **No entanglement with sf.** stars is deeply coupled to sf for spatial operations. The proposed design keeps spatial concerns optional.

What stars does well that should be preserved: the idea that you can `st_apply()` a function across dimensions, and the use of `units` for physical quantities.


## 6. Integration Points

tidync's NetCDF schema introspection could feed directly into the Variable/DataArray metadata layer. Its `hyper_filter()` → `hyper_tibble()` pipeline is a specific materialization path that could coexist with the more general `sel()` → `compute()` path.

The cell abstraction from raster/terra (dimension, extent, resolution) is directly relevant for spatial coordinate handling. Packages like vaster, gdalraster, and terra all provide versions of this logic — any of them could inform the `ImplicitCoord` implementation for regular grids. A GDAL backend for the LazyArray would call into vapour or gdalraster for raster reads.

dibble provides the right abstraction for small in-memory cubes with named broadcasting, and an `as_dibble()` method on DataArray would connect the two worlds: dibble for interactive exploration of modest cubes, the proposed design for lazy access to multi-file terabyte datasets.

The proposed design doesn't replace any of these — it provides a unifying dimension-aware layer on top.


## 7. Risks and Open Questions

**R's array semantics.** R arrays are always eager and in-memory. There's no built-in lazy array type. The LazyArray class needs to be carefully designed so that accidental materialization (e.g., `print(x)` calling `str()` which calls `dim()` which forces evaluation) doesn't happen. The dbplyr approach of having a completely separate class that only materializes on explicit `collect()` is the safest model.

**Memory model.** R copies on modify. xarray's Variable is thin — it holds a reference to the backing array. In R, if the backing array is an R array, assigning `var$data <- new_data` may trigger a copy. Using environments or R6 with reference semantics can mitigate this.

**Performance.** R's indexing is column-major (Fortran order), xarray/numpy is row-major (C order) by default, and NetCDF/Zarr can be either. The dimension permutation on read needs to be handled carefully to avoid unnecessary transposition.

**Community adoption.** The R spatial community has significant inertia around terra and stars. A new package needs to demonstrate clear value-add (multi-file lazy access, named-dimension broadcasting) without requiring users to abandon their existing tools.

**Arrow dependency weight.** If Arrow is adopted as the chunk representation, the dependency strategy needs care. The full `arrow` R package pulls in Arrow C++ (~50MB). nanoarrow is lightweight (~1MB, zero dependencies) but has narrower functionality. A pragmatic path: nanoarrow in the core for zero-copy interchange and basic array handling, with the full arrow package as a suggested dependency for compute kernels and compression codecs. This parallels how dbplyr core is lightweight but gains power when connected to DuckDB or Spark.
