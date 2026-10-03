# Align a Variable's data to a target set of named dimensions

Permutes existing dims to match target order and inserts size-1 dims
where the Variable is missing a dimension. Returns a raw R array with
dim matching the target (with 1s for missing dims).

## Usage

``` r
align_data(v, target_dims)
```

## Arguments

- v:

  A Variable

- target_dims:

  Character vector of dimension names (the output order)

## Value

An R array with dim attribute set
