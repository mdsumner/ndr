# Combine two regular coordinates into an AffineIndex

Replaces the two
[ImplicitCoord](https://mdsumner.github.io/ndr/reference/ImplicitCoord.md)s
for `dims` with one
[AffineIndex](https://mdsumner.github.io/ndr/reference/AffineIndex.md),
so the grid and its CRS are carried, selected and compared as one thing.

## Usage

``` r
set_affine_index(x, dims = c("x", "y"), crs = character())
```

## Arguments

- x:

  A DataArray or Dataset

- dims:

  Character, length 2: the column (x) dim then the row (y) dim.

- crs:

  Optional CRS string to attach.

## Value

`x` with its coords updated

## Examples

``` r
v <- Variable(dims = c("lon", "lat"), data = matrix(1:12, 4, 3))
da <- DataArray(variable = v, coords = list(
  lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100.5, step = 1),
  lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -40.5, step = -1)
))
set_affine_index(da, c("lon", "lat"), crs = "EPSG:4326")
#> <DataArray> '(unnamed)'
#>   Dimensions:  (lon: 4, lat: 3)
#>   Coordinates:
#>     * lon,lat  (lon, lat) affine 4 x 3, res 1, -1, crs EPSG:4326
#>   dtype: integer  (48 B)
```
