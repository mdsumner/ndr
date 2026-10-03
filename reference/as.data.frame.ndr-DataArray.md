# Coerce to data frame (long format)

Expands a DataArray into a data frame with one row per cell, including
coordinate values for each dimension. Compatible with ggplot2 and dplyr.

## Usage

``` r
# S3 method for class '`ndr::DataArray`'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A DataArray

- row.names:

  Ignored

- optional:

  Ignored

- ...:

  Ignored

## Value

A data.frame

## Examples

``` r
da <- DataArray(
  variable = Variable(
    dims = c("lat", "lon"),
    data = matrix(1:6, 2, 3)
  ),
  coords = list(
    lat = ExplicitCoord(dimension = "lat", values = c(10, 20)),
    lon = ExplicitCoord(dimension = "lon", values = c(100, 110, 120))
  ),
  name = "value"
)
as.data.frame(da)
#>   lat lon value
#> 1  10 100     1
#> 2  20 100     2
#> 3  10 110     3
#> 4  20 110     4
#> 5  10 120     5
#> 6  20 120     6
```
