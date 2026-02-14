# ndr Backend Roadmap

Status of components that align with ndr, and what they need.

*Last updated: 2026-02-14. gdalraster multidim-api branch (a698eb5) is
feature-complete for ndr backend integration. All originally identified
issues resolved. The backend is ready to build.*


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


## 1. gdalraster multidim-api branch — ✅ READY

### API surface (Feb 2026, a698eb5)

Complete end-to-end multidim read pipeline. Tested against local NetCDF (OISST)
and remote kerchunk-parquet (BRAN2023 12TB via `/vsicurl/`).

| Function / Method | Status | Notes |
|-------------------|--------|-------|
| `new(GDALMultiDimRaster, dsn, ...)` | ✅ | Opens any GDAL-supported multidim source |
| `ds$getArrayNames()` | ✅ | Lists all arrays including coords |
| `ds$openArrayFromFullname(path, opts)` | ✅ | Returns MDArray pointer |
| `ds$getRootGroup()` | ✅ | Returns GDALGroup pointer |
| `ds$close()` | ✅ | |
| `mdim_array_info(arr)` | ✅ | dim_names, shape, dtype, unit, nodata, scale, offset |
| `mdim_array_read(arr, start, count, step, decode)` | ✅ | Flat vector + `$gis` attribute |
| `mdim_dim_values(arr, dim_index)` | ✅ | Coordinate values for a dimension |
| `mdim_coord_info(arr, dim_index)` | ✅ | Dim metadata including CF units + calendar |
| `mdim_array_attr_names(arr)` | ✅ | List attribute names |
| `mdim_array_attr(arr, name)` | ✅ | Read attribute (string or numeric) |
| `mdim_group_attr_names(group)` | ✅ | List group attribute names |
| `mdim_group_attr(group, name)` | ✅ | Read group attribute |
| `mdim_group_attrs(group)` | ✅ | All group attributes as named list |

### Output convention: `$gis` attribute

`mdim_array_read()` returns a flat R vector (integer, numeric, or raw depending
on source type) with a `$gis` attribute carrying all dimensional metadata.
Matches gdalraster's existing `read_ds()` convention.

```r
x <- mdim_array_read(arr, start = c(0,0,0), count = c(1,180,360))
gis <- attr(x, "gis")
# gis$type      = "multidim"
# gis$dim       = c(360, 180, 1)           — R (F-order), reversed from GDAL
# gis$dim_names = c("lon", "lat", "time")  — matches dim order
# gis$coords    = list(lon=..., lat=..., time=...)  — subsetted to match read
# gis$datatype  = "Int16"
# gis$nodata    = 32767
# gis$scale     = 0.01
# gis$offset    = 0
# gis$bbox      = c(0, -90, 360, 90)       — if spatial dims present
# gis$srs       = "..."                     — if CRS present
```

Key design decisions (documented in `mdim-api-design-notes.md`):

- **Dim order in R order**: `$gis$dim` and `$gis$dim_names` are reversed from
  GDAL's C-order. `array(x, gis$dim)` produces correct R array indexing.
- **Coords subsetted**: `$gis$coords` contains only coordinates matching the
  hyperslab read, not full dimension extent.
- **Type mapping**: Byte→raw, Int8–Int32→integer, UInt32+/Float→numeric.
- **CF decode**: `decode=TRUE` (default) applies scale/offset and nodata→NA.
- **Hyperslab**: start is 0-based, count is output size, step is source stride.
  All in GDAL order (reversed from R dim order).

### Mapping to ndr backend

The `$gis` attribute makes the mapping direct — no `aperm()`, no name reversal,
no manual coord slicing:

```r
nd_read_gdal <- function(ds, var, start = NULL, count = NULL) {
  arr <- ds$openArrayFromFullname(paste0("/", var), character())
  x <- mdim_array_read(arr, start = start, count = count, decode = TRUE)
  gis <- attr(x, "gis")
  Variable(dims = gis$dim_names, data = array(x, dim = gis$dim))
}

nd_schema_gdal <- function(dsn) {
  ds <- new(GDALMultiDimRaster, dsn, TRUE, character(), FALSE)
  array_names <- ds$getArrayNames()
  vars <- list(); coords <- list()

  for (nm in array_names) {
    arr <- ds$openArrayFromFullname(paste0("/", nm), character())
    info <- mdim_array_info(arr)

    if (length(info$dim_names) == 1L && info$dim_names == nm) {
      # Coordinate array
      vals <- mdim_dim_values(arr, 0)
      ci <- mdim_coord_info(arr, 0)
      # Detect time and decode if CF units present
      if (ci$type == "TEMPORAL" && !is.null(ci$units)) {
        vals <- cf_decode_time(vals, ci$units, ci$calendar)
      }
      coords[[nm]] <- if (is_regular(vals)) {
        ImplicitCoord(dimension = nm, n = length(vals),
                      offset = vals[1], step = diff(vals[1:2]))
      } else {
        ExplicitCoord(dimension = nm, values = vals)
      }
    } else {
      # Data variable
      all_attrs <- list(unit = info$unit)
      for (a in mdim_array_attr_names(arr)) {
        all_attrs[[a]] <- mdim_array_attr(arr, a)
      }
      vars[[nm]] <- list(dims = info$dim_names, dtype = info$dtype,
                         attrs = all_attrs)
    }
  }

  # Group attrs
  root <- ds$getRootGroup()
  global_attrs <- mdim_group_attrs(root)
  ds$close()

  list(dims = ..., coords = coords, vars = vars, attrs = global_attrs)
}
```

### Issue tracking

See `gdalraster-mdim-api-requests.md` for the full history. Summary:

| # | Issue | Resolution |
|---|-------|-----------|
| 1 | Dim order reversal | ✅ Documented convention — R order in `$gis` |
| 2 | String attrs fail | ✅ Fixed df7bd08ea |
| 3 | Group-level attrs | ✅ Added df7bd08ea |
| 4 | Subgroup navigation | Open — not needed yet, low-level API exists |
| 5 | Step/stride | ✅ Always worked |
| 6 | $gis attribute | ✅ Added a698eb5 |

See `mdim-api-design-notes.md` for detailed design rationale.


## 2. GDAL7 SWIG bindings (longer term)

The SWIG route would expose the same C++ API but auto-generated. Advantages:
complete API coverage, stays in sync with GDAL releases, mirrors Python bindings.
Disadvantages: GDAL7 not released yet, SWIG R code is not idiomatic, build complexity.

The ndr backend contract doesn't care which is underneath. gdalraster multidim
is the near-term path; GDAL7 SWIG is the escape hatch if hand-wrapping becomes
unsustainable.


## 3. tidync (complementary, NetCDF-only)

tidync is already complete for its scope. The bridge to ndr is thin:

```r
as_dataset.tidync <- function(x, ...) {
  coords <- lapply(x$dimension, function(d) {
    vals <- d$values
    if (is_regular(vals)) {
      ImplicitCoord(dimension = d$name, n = length(vals),
                    offset = vals[1], step = diff(vals[1:2]))
    } else {
      ExplicitCoord(dimension = d$name, values = vals)
    }
  })
  data_vars <- lapply(x$variable, function(v) {
    Variable(dims = v$dim_names, data = array(NA, dim = v$dim_sizes),
             attrs = v$attrs)
  })
  Dataset(data_vars = data_vars, coords = coords, attrs = x$attrs)
}
```

Low priority — GDAL covers the same files. Useful for existing tidync users.


## 4. Arrow (interchange, not engine)

Not a file-reading backend. Arrow's role is zero-copy interchange,
compression-aware reading, and compute kernels for reductions. Becomes
interesting when data is already in Arrow format or memory pressure requires
avoiding copies. Not blocking for ndr 0.x.


## 5. What ndr itself needs to support backends

ndr 0.1.0 is pure in-memory. To support backends:

| Feature | Purpose | Complexity | Status |
|---------|---------|------------|--------|
| `open_dataset()` generic | Entry point dispatching to backends | Low | Ready to build |
| Backend registration | S7 dispatch on source type | Low | Ready to build |
| CF time decode | Parse "days since ..." to R Date/POSIXct | Low | Ready to build |
| Lazy Variable | `@data` is a callback, not an array | Medium | Future |
| Chunk-aware read | `sel()`/`isel()` calls `nd_read()` on demand | Medium | Future |

An eager `open_dataset()` that reads everything into memory is the immediate
next step. Lazy evaluation comes later.


## Summary: what blocks what

```
ndr 0.1.0 (done, 82 tests)
  │
  ├─ gdalraster multidim-api (a698eb5) ✅ READY
  │   ├─ nd_schema(): ✅ mdim_array_info + mdim_dim_values + mdim_coord_info
  │   ├─ nd_read():   ✅ mdim_array_read with $gis attribute
  │   ├─ attrs:       ✅ mdim_array_attr + mdim_group_attrs
  │   └─ all issues resolved, design documented
  │
  ├─ tidync bridge: could build today, low priority
  │
  └─ ndr open_dataset()  ← NEXT STEP
      ├─ eager mode: read all into memory via gdalraster — build now
      ├─ CF time decode: parse units string to R Date — build now
      └─ lazy mode: read on sel/isel demand — build later
```

Next concrete step: implement `open_dataset()` in ndr calling gdalraster
multidim, returning a Dataset with coordinates and variables populated.
Start eager (read everything), add lazy evaluation once the API is proven.
