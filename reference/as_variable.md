# Quick Variable constructor from a named-dim array

Quick Variable constructor from a named-dim array

## Usage

``` r
as_variable(data, ...)
```

## Arguments

- data:

  An R array

- ...:

  Dimension names as `name = size` pairs (ignored, names used) OR a
  character vector of dim names

## Value

A Variable

## Examples

``` r
# From a matrix with named dims
v <- as_variable(matrix(1:12, 3, 4), lat = 3, lon = 4)
```
