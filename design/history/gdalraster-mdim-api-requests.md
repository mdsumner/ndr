# gdalraster multidim-api: Requests & Issues

Living document for communication between the ndr project and the gdalraster
multidim-api team. Items are added as discovered through ndr integration work
and testing. Items are marked resolved when fixed in the branch.

Branch: https://github.com/mdsumner/gdalraster/tree/gdalmultidim-api


## Status summary

| # | Issue | Severity | Status |
|---|-------|----------|--------|
| 1 | ~~Dimension order reversal~~ | ~~Medium~~ | ✅ Resolved — documented convention |
| 2 | ~~String attribute reading fails~~ | ~~Low~~ | ✅ Resolved df7bd08ea |
| 3 | ~~Group-level attributes~~ | ~~Low~~ | ✅ Resolved df7bd08ea |
| 4 | Subgroup navigation | Low | Open — not needed yet |
| 5 | ~~Step/stride in mdim_array_read~~ | ~~Low~~ | ✅ Resolved — always worked |
| 6 | ~~$gis attribute for output~~ | ~~Medium~~ | ✅ Resolved — matches read_ds() |


## Open issues

### 4. Subgroup navigation

**Observed:** Some formats (HDF5, Zarr v3) have nested group hierarchies.
Currently `ds$getArrayNames()` lists arrays from the root group only.

**Impact:** Not needed yet. Most NetCDF, Zarr v2, and kerchunk-parquet stores
are flat. This becomes relevant for HDF-EOS, multi-group HDF5, and Zarr v3
with hierarchical groups.

**Note:** The module already exposes `ds$getGroupNames()`, `ds$openSubGroup()`,
and the full `GDALGroupR` class. This is already implementable, just not
wrapped in convenience functions.

No rush on this.


## Resolved issues

### 1. Dimension order reversal ✅

**Observed:** `mdim_array_read()` returns dimensions in reversed order compared
to what `mdim_array_info()` reports.

**Resolution:** This is inherent to C-order (GDAL) vs F-order (R) memory layout.
The `$gis$dim` and `$gis$dim_names` are now in **R order** (reversed from GDAL),
so they match the actual R array structure. This is the correct behavior.

```r
# GDAL reports: time(494), lat(180), lon(360)  [C-order]
# $gis$dim:     c(360, 180, 494)               [R F-order]
# $gis$dim_names: c("lon", "lat", "time")      [R F-order]

x <- mdim_array_read(arr)
length(x) == prod(attr(x, "gis")$dim)  # TRUE
```

Documented in mdim-api-design-notes.md.


### 2. String attribute reading ✅

**Resolved in:** df7bd08ea

**Was:** `mdim_array_attr(arr, "long_name")` errored with "Array data type is
not convertible to buffer data type".

**Fix:** Check `GDALExtendedDataType::GetClass()` first. GEDTC_STRING reads as
string, GEDTC_NUMERIC reads as numeric.

```r
mdim_array_attr(arr, "long_name")
# [1] "Monthly Mean of Sea Surface Temperature"
```


### 3. Group-level attributes ✅

**Resolved in:** df7bd08ea

**Was:** No way to access NetCDF global attributes (title, history, Conventions).

**Fix:** Added `mdim_group_attr_names()`, `mdim_group_attr()`, `mdim_group_attrs()`.

```r
root <- ds$getRootGroup()
mdim_group_attrs(root)
# $title
# [1] "NOAA Optimum Interpolation (OI) SST V2"
# $Conventions
# [1] "CF-1.0"
```


### 5. Step/stride support ✅

**Status:** Always worked. Follows HDF5/NetCDF hyperslab semantics:

- **count** = number of elements in OUTPUT
- **step** = stride in SOURCE (read every Nth element)

```r
# Read every 10th lat/lon (count=18,36 gives output size, step=10 is stride)
x <- mdim_array_read(arr, start=c(0,0,0), count=c(1,18,36), step=c(1,10,10))
attr(x, "gis")$dim  # [1] 36 18 1
```

Natural count for full extent at stride: `floor((dim_size - start - 1) / step) + 1`


### 6. $gis attribute for output ✅

**Added:** `mdim_array_read()` now returns flat vector with `$gis` attribute
matching `read_ds()` convention.

```r
x <- mdim_array_read(arr, start=c(0,0,0), count=c(1,180,360))
class(x)  # "numeric" or "integer" or "raw"
str(attr(x, "gis"))
# List of ~10
#  $ type     : chr "multidim"
#  $ dim      : num [1:3] 360 180 1
#  $ dim_names: chr [1:3] "lon" "lat" "time"
#  $ coords   : List of 3 (subsetted to match read)
#  $ srs      : chr ""
#  $ datatype : chr "Int16"
#  $ nodata   : num 32767
#  $ scale    : num 0.01
#  $ offset   : num 0
#  $ bbox     : num [1:4] 0 -90 360 90
```

**Type mapping:**
- Byte → raw
- Int8/Int16/UInt16/Int32 → integer
- UInt32/Int64/UInt64/Float32/Float64 → numeric

**bbox:** Computed from HORIZONTAL_X/Y dimensions, adjusted from cell centers
to cell edges.


### New: mdim_coord_info() ✅

**Added in:** df7bd08ea

Returns dimension metadata including CF units for time decoding:

```r
mdim_coord_info(arr, 0)
# $name     [1] "time"
# $size     [1] 494
# $type     [1] "TEMPORAL"
# $direction [1] ""
# $has_coord_var [1] TRUE
# $units    [1] "days since 1800-1-1 00:00:00"
# $calendar NULL
```


## Current API surface

| Function / Method | Status | Notes |
|-------------------|--------|-------|
| `new(GDALMultiDimRaster, dsn, ...)` | ✅ | Opens any GDAL-supported multidim source |
| `ds$getArrayNames()` | ✅ | Lists all arrays including coords |
| `ds$openArrayFromFullname(path, opts)` | ✅ | Returns MDArray pointer |
| `ds$getRootGroup()` | ✅ | Returns GDALGroup pointer |
| `ds$close()` | ✅ | |
| `mdim_array_info(arr)` | ✅ | dim_names, shape, dtype, unit, nodata, scale, offset |
| `mdim_array_read(arr, start, count, step, decode)` | ✅ | Flat vector + $gis attribute |
| `mdim_dim_values(arr, dim_index)` | ✅ | Coordinate values for a dimension |
| `mdim_coord_info(arr, dim_index)` | ✅ | Dim metadata including CF units + calendar |
| `mdim_array_attr_names(arr)` | ✅ | List attribute names |
| `mdim_array_attr(arr, name)` | ✅ | Read attribute (string or numeric) |
| `mdim_group_attr_names(group)` | ✅ | List group attribute names |
| `mdim_group_attr(group, name)` | ✅ | Read group attribute |
| `mdim_group_attrs(group)` | ✅ | All group attributes as named list |


## ndr backend readiness

The ndr backend can now:

- ✅ Read array data with CF decoding (`mdim_array_read`)
- ✅ Get dimension names and sizes (`$gis$dim`, `$gis$dim_names`)
- ✅ Get coordinate values (`$gis$coords`, subsetted to match read)
- ✅ Detect time coordinates and their CF units (`mdim_coord_info`)
- ✅ Read all variable attributes including strings (`mdim_array_attr`)
- ✅ Read global/group attributes (`mdim_group_attrs`)
- ✅ Access CF encoding params for raw decode (`$gis$nodata/scale/offset`)

**Backend is complete for initial integration.**


## Wishlist (not blocking anything)

### W1. mdim_dataset_info() convenience function

Return a complete schema in one call. Lower priority now that the individual
functions cover the same ground.

### ~~W2. Coordinate type detection~~

✅ Addressed by `mdim_coord_info()`.

### W3. GetView() for numpy-style slicing

```r
view <- mdim_get_view(arr, "[0,10,:,:]")
data <- mdim_array_read(view)
```

Nice for interactive use, not needed for ndr.

### W4. Step-only convenience mode

Auto-compute natural count when only step is provided:

```r
# Current: must calculate count manually
mdim_array_read(arr, start=c(0,0,0), count=c(1,18,36), step=c(1,10,10))

# Wishlist: step-only computes natural count automatically
mdim_array_read(arr, step=c(1,10,10))  # count inferred from dim/step
```

Needs design decision on sentinel values (NULL? missing?).


## Design documentation

See `mdim-api-design-notes.md` for detailed notes on:
- Output structure decisions (flat vector, $gis attribute)
- Type mapping rationale
- Dimension ordering (C-order vs F-order)
- Hyperslab API semantics (start/count/step)
- Array reconstruction for plotting
- CF decoding behavior
- Bounding box computation
- Open questions for future work


## How to use this document

- ndr side adds issues as discovered during integration work
- multidim-api team reviews, implements fixes, moves items to "Resolved"
- New items get the next available number
- Severity: **High** = blocks ndr backend, **Medium** = requires workaround,
  **Low** = cosmetic or enhancement
