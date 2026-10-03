# Dataset: a collection of aligned Variables on shared dimensions

A Dataset holds multiple data variables that share a common set of
dimensions and coordinates. The alignment constraint: any two variables
that share a dimension name must agree on its size.

## Usage

``` r
Dataset(data_vars = list(), coords = list(), attrs = list(), .backend = NULL)
```

## Arguments

- data_vars:

  Named list of Variable objects.

- coords:

  Named list of coordinate objects (ImplicitCoord or ExplicitCoord).

- attrs:

  Named list of global metadata.

- .backend:

  Backend reader (internal, created by
  [`open_dataset()`](https://mdsumner.github.io/ndr/reference/open_dataset.md)).

## Examples

``` r
lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
lon <- ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)

ds <- Dataset(
  data_vars = list(
    temperature = Variable(
      dims = c("lat", "lon"),
      data = matrix(rnorm(180*360), 180, 360),
      attrs = list(units = "K")
    ),
    pressure = Variable(
      dims = c("lat", "lon"),
      data = matrix(rnorm(180*360), 180, 360),
      attrs = list(units = "Pa")
    )
  ),
  coords = list(lat = lat, lon = lon),
  attrs = list(title = "Example dataset")
)
ds
#> <Dataset>
#>   Dimensions:  (lat: 180, lon: 360)
#>   Coordinates:
#>     * lat  (lat) -89.5 to 89.5
#>     * lon  (lon) 0.5 to 359.5
#>   Data variables:
#>     temperature          (lat, lon) double
#>     pressure             (lat, lon) double
#>   Attributes:
#>     title: Example dataset
```
