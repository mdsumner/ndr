
# ndr

Named-dimension arrays with coordinate-based indexing for R.

The core idea: put **names on dimensions** and **coordinates on axes**,
then make every operation respect those names. Arithmetic broadcasts by
dimension name, not position.

## Installation

``` r
## install.packages("remotes")
remotes::install_github("mdsumner/ndr")
```

## Variables

A `Variable` is an N-dimensional array that knows its dimension names.

``` r
library(ndr)

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
```

Scalars work too:

``` r
s <- Variable(dims = character(), data = array(42))
s
#> <Variable> scalar double
#>   value: 42
```

## Coordinates

`ImplicitCoord` defines regular grids with zero memory — just offset,
step, and count. `ExplicitCoord` holds arbitrary coordinate values.

``` r
lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
coord_values(lat)
#>   [1] -89.5 -88.5 -87.5 -86.5 -85.5 -84.5 -83.5 -82.5 -81.5 -80.5 -79.5 -78.5
#>  [13] -77.5 -76.5 -75.5 -74.5 -73.5 -72.5 -71.5 -70.5 -69.5 -68.5 -67.5 -66.5
#>  [25] -65.5 -64.5 -63.5 -62.5 -61.5 -60.5 -59.5 -58.5 -57.5 -56.5 -55.5 -54.5
#>  [37] -53.5 -52.5 -51.5 -50.5 -49.5 -48.5 -47.5 -46.5 -45.5 -44.5 -43.5 -42.5
#>  [49] -41.5 -40.5 -39.5 -38.5 -37.5 -36.5 -35.5 -34.5 -33.5 -32.5 -31.5 -30.5
#>  [61] -29.5 -28.5 -27.5 -26.5 -25.5 -24.5 -23.5 -22.5 -21.5 -20.5 -19.5 -18.5
#>  [73] -17.5 -16.5 -15.5 -14.5 -13.5 -12.5 -11.5 -10.5  -9.5  -8.5  -7.5  -6.5
#>  [85]  -5.5  -4.5  -3.5  -2.5  -1.5  -0.5   0.5   1.5   2.5   3.5   4.5   5.5
#>  [97]   6.5   7.5   8.5   9.5  10.5  11.5  12.5  13.5  14.5  15.5  16.5  17.5
#> [109]  18.5  19.5  20.5  21.5  22.5  23.5  24.5  25.5  26.5  27.5  28.5  29.5
#> [121]  30.5  31.5  32.5  33.5  34.5  35.5  36.5  37.5  38.5  39.5  40.5  41.5
#> [133]  42.5  43.5  44.5  45.5  46.5  47.5  48.5  49.5  50.5  51.5  52.5  53.5
#> [145]  54.5  55.5  56.5  57.5  58.5  59.5  60.5  61.5  62.5  63.5  64.5  65.5
#> [157]  66.5  67.5  68.5  69.5  70.5  71.5  72.5  73.5  74.5  75.5  76.5  77.5
#> [169]  78.5  79.5  80.5  81.5  82.5  83.5  84.5  85.5  86.5  87.5  88.5  89.5

coord_lookup(lat, 0)
#> [1] 91

time <- ExplicitCoord(
  dimension = "time",
  values = as.Date("2020-01-01") + 0:364
)
coord_length(time)
#> [1] 365
```

## DataArrays

A `DataArray` wraps a `Variable` with coordinates for label-based
access.

``` r
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

## Broadcasting

Arithmetic broadcasts by dimension name. A 3D temperature field times a
2D land mask just works — dimensions are aligned by name, not position.

``` r
temp <- Variable(
  dims = c("time", "lat", "lon"),
  data = array(rnorm(10 * 3 * 4), dim = c(10, 3, 4))
)
mask <- Variable(
  dims = c("lat", "lon"),
  data = matrix(c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0), 3, 4)
)

result <- temp * mask
shape(result)
#> time  lat  lon 
#>   10    3    4

temp + 273.15
#> <Variable> (time: 10, lat: 3, lon: 4) double
temp * 2
#> <Variable> (time: 10, lat: 3, lon: 4) double
```

## Indexing

`sel()` selects by coordinate value. `isel()` selects by integer index.

``` r
v <- Variable(dims = c("lat", "lon"), data = matrix(1:12, 3, 4))
da <- DataArray(
  variable = v,
  coords = list(
    lat = ImplicitCoord(dimension = "lat", n = 3L, offset = -10, step = 10),
    lon = ImplicitCoord(dimension = "lon", n = 4L, offset = 100, step = 10)
  )
)

sel(da, lat = 0)
#> <DataArray> '(unnamed)'
#>   Dimensions:  (lon: 4)
#>   Coordinates:
#>     * lon  (lon) 100 to 130
#>   dtype: integer  (16 B)
sel(da, lat = c(-10, 0))
#> <DataArray> '(unnamed)'
#>   Dimensions:  (lat: 2, lon: 4)
#>   Coordinates:
#>     * lat  (lat) -10 to 0
#>     * lon  (lon) 100 to 130
#>   dtype: integer  (32 B)
isel(da, lon = 1:2)
#> <DataArray> '(unnamed)'
#>   Dimensions:  (lat: 3, lon: 2)
#>   Coordinates:
#>     * lat  (lat) -10 to 10
#>     * lon  (lon) 100 to 110
#>   dtype: integer  (24 B)
isel(da, lat = 2, lon = 3)
#> <DataArray> '(unnamed)'
#>   dtype: integer  (4 B)
```

## Reductions

Reduce along named dimensions with `nd_mean()`, `nd_sum()`, `nd_min()`,
`nd_max()`.

``` r
temp <- Variable(
  dims = c("time", "lat", "lon"),
  data = array(rnorm(10 * 3 * 4), c(10, 3, 4))
)

nd_mean(temp, "time")
#> <Variable> (lat: 3, lon: 4) double
nd_mean(temp, c("lat", "lon"))
#> <Variable> (time: 10) double
nd_mean(temp, c("time", "lat", "lon"))
#> <Variable> scalar double
#>   value: -0.1608415
```

## Datasets

A `Dataset` holds multiple aligned variables on shared dimensions.

``` r
lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0)
lon <- ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)

ds <- Dataset(
  data_vars = list(
    temperature = Variable(
      dims = c("lat", "lon"),
      data = matrix(rnorm(180 * 360), 180, 360),
      attrs = list(units = "K")
    ),
    pressure = Variable(
      dims = c("lat", "lon"),
      data = matrix(rnorm(180 * 360), 180, 360),
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

## To data frame

`as.data.frame()` on a DataArray gives long-format output for
ggplot2/dplyr.

``` r
da <- DataArray(
  variable = Variable(dims = c("lat", "lon"), data = matrix(1:6, 2, 3)),
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

## Scope

This is the foundation layer: named-dimension arrays with correct
broadcasting, coordinate-based indexing, and reductions. No file
backends, no lazy evaluation, no chunked compute. Those come later as
separate packages that build on this.

## Code of Conduct

Please note that the ndr project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
