# ndr

Named-dimension arrays with coordinate-based indexing for R.

The core idea: put **names on dimensions** and **coordinates on axes**, then
make every operation respect those names. Arithmetic broadcasts by dimension
name, not position.

## Install

```r
# install.packages("S7")  # if needed
devtools::install_local("path/to/ndr")
```

## Quick tour

```r
library(ndr)

# A 3D temperature field
temp <- Variable(
  dims = c("time", "lat", "lon"),
  data = array(rnorm(10 * 180 * 360), c(10, 180, 360)),
  attrs = list(units = "K", long_name = "Surface Temperature")
)
temp
#> <Variable> (time: 10, lat: 180, lon: 360) double
#>   Attributes:
#>     units: K
#>     long_name: Surface Temperature

# A 2D land mask — note: different dims, different order
mask <- Variable(
  dims = c("lat", "lon"),
  data = matrix(sample(0:1, 180*360, replace = TRUE), 180, 360)
)

# Broadcasting aligns by name, not position
masked <- temp * mask
shape(masked)
#> time  lat  lon
#>   10  180  360

# Attach coordinates — implicit for regular grids (zero memory)
da <- DataArray(
  variable = temp,
  coords = list(
    time = ExplicitCoord(dimension = "time", values = as.Date("2020-01-01") + 0:9),
    lat  = ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1.0),
    lon  = ImplicitCoord(dimension = "lon", n = 360L, offset = 0.5, step = 1.0)
  ),
  name = "temperature"
)
da
#> <DataArray> 'temperature'
#>   Dimensions:  (time: 10, lat: 180, lon: 360)
#>   Coordinates:
#>     * time  (time) Date 2020-01-01 ... 2020-01-10
#>     * lat   (lat) -89.5 to 89.5
#>     * lon   (lon) 0.5 to 359.5
#>   dtype: double  (4.9 MB)
#>   Attributes:
#>     units: K
#>     long_name: Surface Temperature

# Select by coordinate value
tropics <- sel(da, lat = c(-23.5, 23.5))
shape(tropics)
#> time  lat  lon
#>   10   47  360

# Select by integer index
day1 <- isel(da, time = 1)
shape(day1)
#>  lat  lon
#>  180  360

# Reduce along named dims
time_mean <- nd_mean(da, "time")
shape(time_mean)
#>  lat  lon
#>  180  360

# To data frame for ggplot2
df <- as.data.frame(day1)
head(df)
#>     lat   lon temperature
#> 1 -89.5   0.5   0.2345...
#> 2 -88.5   0.5  -1.1234...
```

## What this is

The minimal viable core of an R xarray: named dimensions, coordinate-based
indexing, broadcasting by name, and reductions along named dims. Pure R + S7,
no heavy dependencies.

## What this is not (yet)

- No lazy evaluation / file backends (that's the next layer)
- No multi-file aggregation
- No chunked computation
- No Zarr/NetCDF/GDAL IO

Those come later as separate packages that build on this foundation.

## Design

See the companion design document (`xarray-r-design-analysis.md`) for the
full architectural analysis including comparison with xarray, stars, dibble,
rray, and tbl_cube.
