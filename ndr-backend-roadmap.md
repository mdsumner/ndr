# ndr Backend Roadmap

Status of components that are expected to align with ndr, and what they need.

## The minimal backend contract

A backend for ndr needs exactly two operations:

```r
nd_schema(source)
# Returns: list(
#   dims = c(Time = 5599L, st_ocean = 51L, yt_ocean = 1500L, xt_ocean = 3600L),
#   coords = list(
#     Time = ExplicitCoord(dimension = "Time", values = <datetime vector>),
#     st_ocean = ExplicitCoord(dimension = "st_ocean", values = <numeric>),
#     ...
#   ),
#   vars = list(
#     temp = list(dims = c("Time", "st_ocean", "yt_ocean", "xt_ocean"),
#                 dtype = "float64",
#                 attrs = list(units = "degrees C", long_name = "Potential temperature"))
#   ),
#   attrs = list(title = "BRAN2023")
# )

nd_read(source, var, slices)
# slices is a named list of integer ranges, e.g. list(Time = 1L, st_ocean = 11L, yt_ocean = 101L)
# Returns: R array with dim set, CF-decoded (scale/offset applied, fill → NA)
```

Everything above this — broadcasting, sel/isel, reductions, print — is ndr's job.
The backend is just a reader.


## 1. gdalraster multidim-api branch

### What exists (Jan 2026)

The branch exposes GDAL's multidim C++ classes via Rcpp:

| R method | GDAL C++ | Status | Notes |
|----------|----------|--------|-------|
| `new(GDALMultiDimRaster, dsn, ...)` | `GDALDataset::Open(OF_MULTIDIM_RASTER)` | ✅ works | Opens any GDAL-supported multidim source |
| `ds$getRootGroup()` | `GDALDataset::GetRootGroup()` | ✅ works | Returns GDALGroup pointer |
| `ds$getArrayNames()` | `GDALGroup::GetMDArrayNames()` | ✅ works | Lists all arrays including coords |
| `ds$openArrayFromFullname(path, opts)` | `GDALGroup::OpenMDArrayFromFullname()` | ✅ works | Returns MDArray pointer |
| `ds$getDriverLongName()` | `GDALDriver::GetDescription()` | ✅ works | |
| `ds$getFileList()` | `GDALDataset::GetFileList()` | ✅ works | |
| `ds$readOnly` | — | ✅ works | Property |

Works with `/vsicurl/` and remote kerchunk-parquet stores already.

### What's needed for nd_schema()

From the **MDArray pointer**, need to expose:

| R method needed | GDAL C++ method | Purpose for ndr |
|----------------|-----------------|-----------------|
| `ar$getDimensionCount()` | `MDArray::GetDimensionCount()` | Number of dims |
| `ar$getDimensions()` | `MDArray::GetDimensions()` | List of GDALDimension objects |
| `ar$getDataType()` | `MDArray::GetDataType()` | Map to R type (double, integer, etc.) |
| `ar$getAttribute(name)` | `MDArray::GetAttribute()` | Individual attr value |
| `ar$getAttributes()` | `MDArray::GetAttributes()` | All attrs → list |
| `ar$getScale()` / `ar$getOffset()` | `MDArray::GetScale()` / `GetOffset()` | CF decoding params |
| `ar$getNoDataValue()` | `MDArray::GetNoDataValueAsDouble()` | _FillValue → NA |
| `ar$getShape()` | `MDArray::GetDimensions()` → sizes | Dim sizes as integer vector |

From each **GDALDimension**, need:

| R method needed | GDAL C++ method | Purpose for ndr |
|----------------|-----------------|-----------------|
| `dim$getName()` | `GDALDimension::GetName()` | Dim name string |
| `dim$getSize()` | `GDALDimension::GetSize()` | Dim length |
| `dim$getIndexingVariable()` | `GDALDimension::GetIndexingVariable()` | The coordinate array for this dim |

The indexing variable is key: it's how you discover that the "Time" dimension
has coordinate values [2024-12-01, 2024-12-02, ...]. Read that once → ExplicitCoord.
For regular grids, detect regularity from the values → ImplicitCoord.

### What's needed for nd_read()

This is the critical missing piece — actually getting data out:

| R method needed | GDAL C++ method | Purpose for ndr |
|----------------|-----------------|-----------------|
| `ar$read(start, count)` | `MDArray::Read(arrayStartIdx, count, arrayStep, bufferStride, bufferDataType, buffer)` | Read a hyperslab into an R array |
| `ar$getView(spec)` | `MDArray::GetView(viewExpr)` | NumPy-style slicing `"[0,10,100,:]"` → subarray |

Either `read()` or `getView()` works. `getView()` is more convenient
(returns a new MDArray that can then be read in full), but `read()` with
start/count is the lower-level primitive. For ndr's `nd_read()`, the mapping is:

```r
nd_read <- function(source, var, slices) {
  ar <- source$openArrayFromFullname(paste0("/", var), "")
  dims <- ar$getDimensions()
  full_shape <- vapply(dims, function(d) d$getSize(), integer(1))

  # build start/count from slices (named list of integer indices)
  start <- rep(0L, length(full_shape))  # 0-based for GDAL
  count <- full_shape
  for (d in names(slices)) {
    i <- match(d, vapply(dims, function(dd) dd$getName(), character(1)))
    start[i] <- min(slices[[d]]) - 1L  # to 0-based
    count[i] <- length(slices[[d]])
  }

  raw <- ar$read(start, count)  # returns numeric vector
  result <- array(raw, dim = count)

  # CF decode
  scale <- ar$getScale()
  offset <- ar$getOffset()
  nodata <- ar$getNoDataValue()
  if (!is.null(nodata)) result[result == nodata] <- NA
  if (!is.null(scale)) result <- result * scale + (offset %||% 0)

  result
}
```

### Priority for gdalraster multidim

1. **MDArray::GetDimensions() exposure** — names and sizes. This unblocks `nd_schema()`.
2. **MDArray::Read() or GetView()** — data extraction. This unblocks `nd_read()`.
3. **GDALDimension::GetIndexingVariable()** — coordinate discovery. This makes coords automatic.
4. **Attribute access** — metadata. Nice to have, not blocking.

Items 1-2 are the minimum viable backend. Item 3 makes it good.


## 2. GDAL7 SWIG bindings (longer term)

The SWIG route would expose the same C++ API but auto-generated, so you'd
get the full surface area without hand-writing Rcpp wrappers. Advantages:

- Complete API coverage by default (every method, not just what's wrapped)
- Stays in sync with GDAL releases automatically
- The Python GDAL bindings use this same SWIG path, so the R API would mirror Python's

Disadvantages:

- GDAL7 isn't released yet
- SWIG-generated R code is not idiomatic
- Build complexity

For now: gdalraster multidim branch is the near-term path. GDAL7 SWIG is
the escape hatch if wrapping individual methods becomes unsustainable.
The ndr backend contract doesn't care which one is underneath.


## 3. tidync (complementary, NetCDF-only)

### What exists

tidync is already complete for its scope:

| tidync function | ndr equivalent | Status |
|----------------|----------------|--------|
| `tidync(file)` | `nd_schema()` — returns full schema | ✅ |
| `hyper_filter()` | coordinate-to-index translation (like `sel()`) | ✅ |
| `hyper_array()` | `nd_read()` — returns R array with dim set | ✅ |
| `hyper_tbl_cube()` | `as.data.frame()` equivalent | ✅ |
| dim/var metadata | attrs, dim names, coord values | ✅ |

### Bridge needed

The tidync → ndr bridge is thin, probably a single function:

```r
as_dataset.tidync <- function(x, ...) {
  # x is a tidync object (already has schema loaded)
  schema <- x  # tidync objects ARE the schema

  # build coords from tidync's dimension info
  coords <- lapply(schema$dimension, function(d) {
    vals <- d$values  # tidync already read these
    if (is_regular(vals)) {
      ImplicitCoord(dimension = d$name, n = length(vals),
                    offset = vals[1], step = diff(vals[1:2]))
    } else {
      ExplicitCoord(dimension = d$name, values = vals)
    }
  })

  # build variable stubs (no data yet)
  data_vars <- lapply(schema$variable, function(v) {
    Variable(dims = v$dim_names,
             data = array(NA, dim = v$dim_sizes),  # placeholder
             attrs = v$attrs)
  })

  Dataset(data_vars = data_vars, coords = coords, attrs = schema$attrs)
}
```

This is already doable today. Low priority because GDAL covers the same
files, but useful for the large existing tidync user base.


## 4. Arrow (interchange, not engine)

### Role

Not a backend in the "read files" sense. Arrow's role:

- **Parquet reading**: kerchunk-parquet stores are Parquet files. Arrow reads them
  natively. But GDAL already handles this path for Zarr access, so Arrow isn't
  needed here unless you want to bypass GDAL.
- **Zero-copy interchange**: if a backend returns Arrow arrays, ndr can hold them
  in `Variable@data` (which accepts `class_any`). No copy on construction.
- **Columnar ops**: Arrow compute kernels (filter, aggregate) could power
  reductions on Arrow-backed Variables without materializing to R arrays.

### When it matters

Arrow becomes interesting when:
- Data is already in Arrow format (Parquet tables, IPC files)
- Memory pressure requires avoiding copies
- Compute kernels are faster than base R for reductions

None of these are blocking for ndr 0.x. Arrow is a performance path, not a
functionality path.


## 5. What ndr itself needs to support backends

ndr 0.1.0 is pure in-memory. To support backends, ndr needs:

| Feature | Purpose | Complexity |
|---------|---------|------------|
| `open_dataset()` generic | Entry point that dispatches to backends | Low — just a generic |
| Backend registration | `register_backend("gdal", gdal_open)` or S3/S7 dispatch | Low |
| Lazy Variable | Variable where `@data` is a promise/callback, not an array | Medium |
| Chunk-aware read | `nd_read()` only fetches the slices needed by `sel()`/`isel()` | Medium |
| CF decode pass | Apply scale/offset/fill after read, before user sees data | Low if backend does it |

The first two are trivial. Lazy Variable is where it gets interesting —
this is where ndr stops being pure in-memory and becomes a front-end
for arbitrary data sources. But even without lazy evaluation, a simple
`open_dataset()` that reads everything into memory (fine for small-to-medium
datasets) is useful and can be implemented immediately once a backend
can do `nd_schema()` + `nd_read()`.


## Summary: what blocks what

```
ndr 0.1.0 (done)
  │
  ├─ gdalraster multidim: needs MDArray read + dimension exposure
  │   ├─ nd_schema(): needs getDimensions(), getAttributes()
  │   ├─ nd_read(): needs Read() or GetView()  ← critical gap
  │   └─ coords: needs getIndexingVariable()
  │
  ├─ tidync bridge: could build today, low priority
  │
  └─ ndr open_dataset() generic: trivial once any backend exists
      └─ lazy Variable: medium effort, needed for large data
          └─ chunk-aware isel/sel: rewire to call nd_read() on demand
```

The critical path is: **gdalraster exposes MDArray::Read()** → ndr gets
`open_dataset()` → the BRAN2023 12TB example works end-to-end in R.
