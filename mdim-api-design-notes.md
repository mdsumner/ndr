# gdalraster multidim-api: Design Notes

Notes from integration work and design discussions. For future reference
when revisiting decisions or adding features.

## Output structure decisions

### Flat vector with $gis attribute

Matches existing `read_ds()` convention for consistency across gdalraster.
The `$gis` attribute carries all spatial/dimensional metadata independent
of base R attributes on the vector itself.

```r
x <- mdim_array_read(arr, ...)
class(x)  # "integer", "numeric", or "raw" depending on source type
str(attr(x, "gis"))
# List of ~10
#  $ type     : chr "multidim"
#  $ dim      : num [1:n] ...
#  $ dim_names: chr [1:n] ...
#  $ coords   : List of n (named by dim_names)
#  $ srs      : chr "..."
#  $ datatype : chr "Int16"
#  $ nodata   : num 32767 (if present)
#  $ scale    : num 0.01 (if present)
#  $ offset   : num 0 (if present)
#  $ bbox     : num [1:4] xmin ymin xmax ymax (if spatial dims found)
```

### Type mapping

| GDAL Type | R Type | Rationale |
|-----------|--------|-----------|
| Byte | raw | Memory efficient |
| Int8, Int16, UInt16, Int32 | integer | Fits R's signed 32-bit |
| UInt32 | numeric | Can exceed INT_MAX (~2.1B) |
| Int64, UInt64 | numeric | No native 64-bit int in R |
| Float32, Float64 | numeric | Natural fit |

**Note:** Byte → raw means no NA support. User must convert to integer/numeric
for NA handling. This matches raster convention where 0 or 255 often encodes
nodata for byte data.

**Future consideration:** bit64 package for true 64-bit integer support.

### Dimension ordering

GDAL multidim uses C-order (row-major): slowest-varying dimension first.
R uses F-order (column-major): fastest-varying dimension first.

The `$gis$dim` and `$gis$dim_names` are in **R order** (reversed from GDAL).
This means `dim(array(x, gis$dim))` produces correct R array indexing.

```r
# GDAL reports: time(494), lat(180), lon(360)
# $gis$dim:     c(360, 180, 494)
# $gis$dim_names: c("lon", "lat", "time")
```

### Coordinate subsetting

When reading a hyperslab, `$gis$coords` contains only the coordinates
for the subset read, not the full dimension. This keeps metadata consistent
with the data.

2D curvilinear coordinates (e.g., rotated grids) are currently read in full.
Future work needed to subset these based on which dimensions match.


## Hyperslab API semantics

Following HDF5/NetCDF conventions:

- **start**: 0-based starting index in each dimension (GDAL order)
- **count**: number of elements to read in OUTPUT (GDAL order)
- **step**: stride in SOURCE - read every Nth element (GDAL order)

Key relationship: `count` IS the output size. `step` affects which source
indices are read but doesn't change output size.

```r
# step=10 reads every 10th element, count=18 means output has 18 elements
mdim_array_read(arr, start=c(0,0,0), count=c(1,18,36), step=c(1,10,10))
```

**Natural count calculation** for "read full extent at stride":
```r
natural_count <- function(dim_size, start = 0, step = 1) {
  floor((dim_size - start - 1) / step) + 1
}
# dim=180, start=0, step=10 → count=18
```

**Future consideration:** Add step-only mode that computes natural count
automatically. Need to decide sentinel values (NULL? missing? NA?) for
"use natural count".


## Array reconstruction for plotting

The flat vector needs reconstruction for image plotting:

```r
x <- mdim_array_read(arr, ...)
gis <- attr(x, "gis")

# Reconstruct as R array
a <- array(x, gis$dim)

# For 2D spatial slice (assuming lon, lat are dims 1, 2):
slice <- a[,,1]  # lon × lat

# For rasterImage/ximage (expects rows=y, cols=x):
# Need transpose and possibly y-flip depending on lat direction
plot_ready <- t(slice)
if (diff(gis$coords$lat)[1] > 0) {
  # South-to-North storage, flip for top-down display
  plot_ready <- plot_ready[nrow(plot_ready):1, ]
}
```

**Future work:**
- ximage helper to handle multidim $gis format
- plot_multidim() in gdalraster for quick visualization
- Standardize on rasterImage convention once patterns stabilize


## CF decoding

`decode=TRUE` (default):
- Applies scale/offset: `value = raw * scale + offset`
- Converts nodata → NA
- Always returns numeric (double)

`decode=FALSE`:
- Returns raw storage values
- Preserves integer/raw type
- `$gis` still includes nodata/scale/offset for manual decode

```r
x_raw <- mdim_array_read(arr, decode = FALSE)
gis <- attr(x_raw, "gis")
# Manual decode:
x_decoded <- as.numeric(x_raw) * gis$scale + gis$offset
x_decoded[x_raw == gis$nodata] <- NA
```


## Bounding box computation

`$gis$bbox` is computed when both HORIZONTAL_X and HORIZONTAL_Y dimensions
are present with 1D coordinate arrays.

Format: `c(xmin, ymin, xmax, ymax)`

Adjusted from cell centers to cell edges by adding/subtracting half the
cell size (computed from first coordinate difference).

Not computed for:
- Curvilinear grids (2D coordinate arrays)
- Non-spatial arrays
- Arrays with only one spatial dimension


## Open questions for future

1. **Sentinel values for optional hyperslab params**: What means "read full
   dimension"? NULL? Missing argument? NA? Affects API ergonomics.

2. **Step-only convenience mode**: Compute natural count from step. Useful
   for downsampling without manual math.

3. **2D coordinate subsetting**: Curvilinear coords need proper hyperslab
   subsetting based on which source dimensions they span.

4. **Time coordinate decoding**: `mdim_coord_info()` returns CF units string.
   Should there be a helper to parse "days since 1800-01-01" and return
   R Date/POSIXct directly?

5. **Dimension order option**: Some users may prefer GDAL order (matching
   Python/xarray). Add `r_order=TRUE` parameter?

6. **Raw type NA handling**: Byte data has no NA. Document workarounds or
   provide helper for nodata → NA conversion that promotes to integer.
