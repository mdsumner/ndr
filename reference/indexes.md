# Indexes: the contract between labels and positions

An index maps coordinate labels to integer positions along one or more
dimensions. It is the object that
[`sel()`](https://mdsumner.github.io/ndr/reference/indexing.md) asks
"which positions?", that
[`isel()`](https://mdsumner.github.io/ndr/reference/indexing.md) asks
"what are you after this subset?", and that arithmetic asks "are these
two objects on the same grid?". The design follows xarray's Index API
(`sel`, `isel`, `equals`, `create_variables`), kept small so that other
packages can implement new kinds of index.

## Usage

``` r
Index()

index_dims(x)

index_sizes(x)

index_coords(x)

index_sel(x, labels)

index_isel(x, positions)

index_drop(x, dims)

index_equals(x, y)
```

## Arguments

- x, y:

  Index objects

- labels:

  Named list of label values, keyed by dimension name

- positions:

  Named list of 1-based integer positions, keyed by dimension name

- dims:

  Character vector of dimension names

## Details

Every entry in a DataArray or Dataset `coords` list is an `Index`.
[ImplicitCoord](https://mdsumner.github.io/ndr/reference/ImplicitCoord.md)
and
[ExplicitCoord](https://mdsumner.github.io/ndr/reference/ExplicitCoord.md)
are one-dimensional indexes;
[AffineIndex](https://mdsumner.github.io/ndr/reference/AffineIndex.md)
owns two dimensions at once through a geotransform.

An index implements these generics:

- `index_dims(x)`: the dimension names it covers.

- `index_sizes(x)`: named integer, the length of each of those dims.

- `index_coords(x)`: named list of coordinate values (one per coordinate
  it owns; a vector for 1D coordinates, a matrix for 2D).

- `index_sel(x, labels)`: `labels` is a named list of label values keyed
  by dimension; returns a named list of 1-based integer positions, one
  element per dimension that was selected on.

- `index_isel(x, positions)`: `positions` is a named list of 1-based
  integer positions keyed by dimension (dims not named are kept whole; a
  single position drops that dim); returns a list of indexes for the
  result (empty when every dim of the index was dropped).

- `index_drop(x, dims)`: list of indexes left when `dims` are removed
  entirely (by a reduction).

- `index_equals(x, y)`: `TRUE` when the two indexes describe the same
  labels.

## Examples

``` r
lat <- ImplicitCoord(dimension = "lat", n = 180L, offset = -89.5, step = 1)
index_dims(lat)
#> [1] "lat"
index_sel(lat, list(lat = c(-10, 10)))
#> $lat
#>  [1]  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
#> [20] 100
#> 
index_equals(lat, ExplicitCoord(dimension = "lat", values = seq(-89.5, 89.5)))
#> [1] TRUE
```
