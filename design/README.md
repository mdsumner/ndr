# Design notes

Not part of the package build (`.Rbuildignore`).

- `xarray-r-design-analysis.md`: the founding analysis, what xarray is and
  what an R version needs.
- `working-sel-examples.R`, `zarr-examples.R`: worked `open_dataset()` and
  `sel()` examples against public stores.
- `history/`: notes from the gdalraster multidim-api era (February 2026):
  the backend roadmap, API requests to that branch, and its design notes.
  Superseded, kept for the record.

## Where things stand

ndr opens multidimensional sources with two engines:

- **GDAL**, through GDAL7 (rgdal-dev/GDAL7, GDAL >= 3.10): anything GDAL's
  multidim API reads (netCDF, HDF5, Zarr, VRT, kerchunk-Parquet). Lazy
  variables come from GDAL7's `as_altarr()`.
- **zaro** (hypertidy/zaro): Zarr V2/V3, kerchunk, VirtualiZarr and
  byterefs stores, read through one pooled fetch (HTTP, and S3/GCS as
  HTTPS when access is known), with no GDAL. Lazy variables come from
  `zaro_array()`.

Lazy data is an altarr array (hypertidy/altarr): a plain R array with a
`dim` that reads chunks when asked, so every method works on it. Grids that
carry an affine transform (GeoZarr `spatial:transform`, a CF grid mapping,
or GDAL's CRS with regular x/y) become one `AffineIndex` instead of two
coordinate arrays. Open work is tracked in hypertidy/ndr#15.
