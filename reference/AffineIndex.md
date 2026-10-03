# Affine (geotransform) index

An `AffineIndex` owns two dimensions at once, the columns (`dims[1]`,
usually x or lon) and rows (`dims[2]`, usually y or lat) of a raster
grid, and maps positions to coordinates with the six numbers of a GDAL
geotransform. Coordinates are never stored: they are computed from the
transform when asked for, as in xarray's `rasterix.RasterIndex`.

## Usage

``` r
AffineIndex(
  dims = character(0),
  shape = integer(0),
  transform = numeric(0),
  crs = character(0)
)
```

## Arguments

- dims:

  Character, length 2: the column dim then the row dim.

- shape:

  Integer, length 2: number of columns, number of rows.

- transform:

  Double, length 6: GDAL geotransform.

- crs:

  Character: CRS as WKT, PROJJSON or an authority code (length 0 when
  unknown).

## Details

The transform is in GDAL order, for pixel corners:
`x = t[1] + col * t[2] + row * t[3]`,
`y = t[4] + col * t[5] + row * t[6]`, so `t[3]` and `t[6]` are the
rotation terms. Coordinates reported by the index are pixel centres.

Selecting a regular (evenly spaced) subset keeps the index affine, with
the transform moved and scaled to match. An irregular subset of a
north-up grid becomes two one-dimensional coordinates. The CRS travels
with the index (one place, not two), and two indexes are only equal when
their CRS agrees as well as their grid.

## Examples

``` r
ix <- AffineIndex(
  dims = c("x", "y"), shape = c(360L, 180L),
  transform = c(-180, 1, 0, 90, 0, -1), crs = "EPSG:4326"
)
index_sel(ix, list(x = c(140, 150), y = c(-45, -40)))
#> $x
#>  [1] 321 322 323 324 325 326 327 328 329 330
#> 
#> $y
#> [1] 131 132 133 134 135
#> 
as_geotransform(index_isel(ix, list(x = 321:330, y = 131:135))[[1]])
#>        origin_x     pixel_width    row_rotation        origin_y column_rotation 
#>             140               1               0             -40               0 
#>    pixel_height 
#>              -1 
```
