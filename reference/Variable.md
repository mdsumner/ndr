# Variable: a named-dimension array

The fundamental building block. A Variable is an N-dimensional array
that knows the names of its dimensions. It does NOT know about
coordinates - that's DataArray's job.

## Usage

``` r
Variable(dims = character(0), data = NULL, attrs = list(), encoding = list())
```

## Arguments

- dims:

  Character vector of dimension names.

- data:

  An R array (or matrix, or vector with dim attribute), or a lazy
  chunked array from the altarr package (see
  [lazy-data](https://mdsumner.github.io/ndr/reference/lazy-data.md)).

- attrs:

  Named list of arbitrary metadata.

- encoding:

  Named list of on-disk encoding info (scale_factor, etc.).

## Examples

``` r
# A 3D temperature field
temp_data <- array(rnorm(365 * 180 * 360), dim = c(365, 180, 360))
v <- Variable(
  dims = c("time", "lat", "lon"),
  data = temp_data,
  attrs = list(units = "K", long_name = "Temperature")
)
v
#> <Variable> (time: 365, lat: 180, lon: 360) double
#>   Attributes:
#>     units: K
#>     long_name: Temperature

# Scalar variable
s <- Variable(dims = character(), data = array(42))
s
#> <Variable> scalar double
#>   value: 42
```
