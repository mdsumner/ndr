# Read lazy data into memory

Replaces lazy (altarr) data with an ordinary in-memory array, reading it
with one planned read. Objects whose data is already in memory are
returned unchanged.

## Usage

``` r
collect(x, ...)
```

## Arguments

- x:

  A Variable, DataArray or Dataset.

- ...:

  Unused.

## Value

An object of the same class with data in memory.

## See also

[lazy-data](https://mdsumner.github.io/ndr/reference/lazy-data.md)
