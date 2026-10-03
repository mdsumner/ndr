# Open a raster band as a lazy DataArray

Opens one band of a classic (2D) GDAL raster, such as a GeoTIFF or COG,
as a DataArray with lazy altarr data and an
[AffineIndex](https://mdsumner.github.io/ndr/reference/AffineIndex.md)
built from the dataset's geotransform and CRS. Nothing but metadata is
read until values are asked for.

## Usage

``` r
open_raster(dsn, band = 1L, dims = c("x", "y"))
```

## Arguments

- dsn:

  Data source name (file path, URL, or GDAL DSN string)

- band:

  Band number (1-based)

- dims:

  Names for the column and row dimensions

## Value

A DataArray with dims `dims` (columns first, as GDAL7 returns them)

## Examples

``` r
if (FALSE) { # \dontrun{
r <- open_raster("/vsicurl/https://example.com/dem.tif")
r  # coords: (x, y) affine ...
r |> sel(x = c(140, 150), y = c(-45, -40)) |> collect()
} # }
```
