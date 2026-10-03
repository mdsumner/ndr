# DataArray: a Variable with coordinates

A DataArray wraps a Variable (the data) and attaches coordinates, so you
can do things like `sel(da, lat = 0, time = "2020-01-15")`. This is the
primary user-facing object for single-variable data.

## Usage

``` r
DataArray(variable = Variable(), coords = list(), name = character(0))
```

## Arguments

- variable:

  A Variable

- coords:

  Named list of
  [indexes](https://mdsumner.github.io/ndr/reference/indexes.md)
  (ImplicitCoord, ExplicitCoord, AffineIndex, ...). Each index's dims
  must be among the Variable's `dims`, with matching sizes.

- name:

  Optional name for this data array (character, length 0 or 1).

## Examples

``` r
# 2D array with implicit spatial coords
v <- Variable(
  dims = c("lat", "lon"),
  data = matrix(rnorm(180 * 360), 180, 360),
  attrs = list(units = "K")
)
da <- DataArray(
  variable = v,
  coords = list(
    lat = ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0),
    lon = ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)
  ),
  name = "temperature"
)
da
#> <DataArray> 'temperature'
#>   Dimensions:  (lat: 180, lon: 360)
#>   Coordinates:
#>     * lat  (lat) -89.5 to 89.5
#>     * lon  (lon) 0.5 to 359.5
#>   dtype: double  (506.2 kB)
#>   Attributes:
#>     units: K
```
