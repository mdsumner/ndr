# Reductions along named dimensions

Summarise a Variable or DataArray by applying a function along one or
more named dimensions. The reduced dimensions are dropped from the
result.

## Usage

``` r
nd_mean(x, dims, na.rm = FALSE)

nd_sum(x, dims, na.rm = FALSE)

nd_min(x, dims, na.rm = FALSE)

nd_max(x, dims, na.rm = FALSE)
```

## Arguments

- x:

  A Variable or DataArray

- dims:

  Character vector of dimension names to reduce over

- na.rm:

  Logical, whether to remove NAs

## Value

Same type as input, with reduced dimensions dropped

## Examples

``` r
temp <- Variable(
  dims = c("time", "lat", "lon"),
  data = array(rnorm(10 * 3 * 4), c(10, 3, 4))
)

# Time mean
nd_mean(temp, "time")  # shape: lat=3, lon=4
#> <Variable> (lat: 3, lon: 4) double

# Spatial mean (reduce lat and lon)
nd_mean(temp, c("lat", "lon"))  # shape: time=10
#> <Variable> (time: 10) double

# Global mean (reduce everything)
nd_mean(temp, c("time", "lat", "lon"))  # scalar
#> <Variable> scalar double
#>   value: -0.1557802
```
