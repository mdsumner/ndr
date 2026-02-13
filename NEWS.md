# ndr 0.1.0

Initial release. Named-dimension arrays with coordinate-based indexing for R.

## Core classes (S7)

* `Variable` — N-dimensional array with named dimensions and attributes.
* `DataArray` — Variable with attached coordinates for label-based access.
* `Dataset` — collection of aligned Variables on shared dimensions. Extract variables with `$` or `[[`.
* `ImplicitCoord` — regular-grid coordinate defined by offset + step (zero memory, O(1) lookup). Slicing preserves implicitness when the subset is regular.
* `ExplicitCoord` — arbitrary coordinate values (numeric, Date, character, etc.).

## Operations

* Arithmetic and comparison operators (`+`, `*`, `>`, etc.) broadcast by dimension name, not position. Disjoint dimensions produce outer-product-style expansion.
* `sel()` — select by coordinate value (label-based, nearest-match for numeric).
* `isel()` — select by integer index (1-based).
* `nd_mean()`, `nd_sum()`, `nd_min()`, `nd_max()` — reduce along one or more named dimensions.

## Coercion

* `as.data.frame()` on DataArray produces long-format output (one row per cell) for use with ggplot2/dplyr.
* `as_variable()` convenience constructor from R arrays.

## Design notes

* Single dependency: S7.
* No file backends, lazy evaluation, or chunked compute — this is the foundation layer for those to build on.
