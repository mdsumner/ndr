# Coordinate-based indexing

`sel()` selects data by coordinate value (label-based). `isel()` selects
data by integer index (position-based).

## Usage

``` r
isel(.data, ...)

sel(.data, ...)
```

## Arguments

- .data:

  A Variable, DataArray, or Dataset

- ...:

  Named arguments specifying the selection. For `sel()`, values are
  coordinate values. For `isel()`, values are integer indices (1-based).
  Use a vector for ranges: `sel(da, lat = c(-30, 30))` selects the
  range. Use a scalar for a single position: `isel(da, time = 1)` picks
  one index and drops that dimension.

## Value

Same type as input, with dimensions sliced or dropped.

## Details

Both work on Variables, DataArrays, and Datasets, and return objects of
the same type with dimensions correctly updated.

## Examples

``` r
v <- Variable(
  dims = c("lat", "lon"),
  data = matrix(1:12, 3, 4)
)
da <- DataArray(
  variable = v,
  coords = list(
    lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10),
    lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100, step = 10)
  )
)

# Select by coordinate value
sel(da, lat = 0)             # single latitude, drops lat dim
#> <DataArray> '(unnamed)'
#>   Dimensions:  (lon: 4)
#>   Coordinates:
#>     * lon  (lon) 100 to 130
#>   dtype: integer  (16 B)
sel(da, lat = c(-10, 0))     # range of latitudes
#> <DataArray> '(unnamed)'
#>   Dimensions:  (lat: 2, lon: 4)
#>   Coordinates:
#>     * lat  (lat) -10 to 0
#>     * lon  (lon) 100 to 130
#>   dtype: integer  (32 B)

# Select by integer index
isel(da, lon = 1:2)          # first two longitude columns
#> <DataArray> '(unnamed)'
#>   Dimensions:  (lat: 3, lon: 2)
#>   Coordinates:
#>     * lat  (lat) -10 to 10
#>     * lon  (lon) 100 to 110
#>   dtype: integer  (24 B)
isel(da, lat = 2, lon = 3)   # single cell
#> <DataArray> '(unnamed)'
#>   dtype: integer  (4 B)
```
