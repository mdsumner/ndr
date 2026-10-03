# Coordinate classes

A Coordinate is a one-dimensional
[Index](https://mdsumner.github.io/ndr/reference/indexes.md). It maps
integer indices to meaningful values along a dimension. Two
representations:

## Details

- **ImplicitCoord**: regular grids defined by offset + step. Coordinates
  are never stored, computed on demand via `offset + (0:(n-1)) * step`.
  This is the raster/terra cell abstraction, and xarray's RangeIndex.

- **ExplicitCoord**: an arbitrary vector of coordinate values (numeric,
  character, POSIXct, or anything with sensible comparison operators).
