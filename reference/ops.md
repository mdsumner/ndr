# Arithmetic operations on Variables and DataArrays

All arithmetic (`+`, `-`, `*`, `/`, etc.) and comparison (`==`, `<`,
etc.) operators are supported. Operations broadcast by dimension name:
dimensions present in one operand but not the other are automatically
expanded.

## Examples

``` r
# Broadcasting: 3D temperature * 2D land mask
temp <- Variable(
  dims = c("time", "lat", "lon"),
  data = array(rnorm(10 * 3 * 4), dim = c(10, 3, 4))
)
mask <- Variable(
  dims = c("lat", "lon"),
  data = matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0), 3, 4)
)

result <- temp * mask
shape(result)  # time=10, lat=3, lon=4
#> time  lat  lon 
#>   10    3    4 

# Scalar operations
temp + 273.15
#> <Variable> (time: 10, lat: 3, lon: 4) double
temp * 2
#> <Variable> (time: 10, lat: 3, lon: 4) double
```
