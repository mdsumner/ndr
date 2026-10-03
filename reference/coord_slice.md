# Slice a coordinate by integer indices

Slice a coordinate by integer indices

## Usage

``` r
coord_slice(x, idx)
```

## Arguments

- x:

  A coordinate

- idx:

  Integer indices (1-based)

## Value

A new coordinate of the same type (ImplicitCoord stays implicit if the
slice is a contiguous regular subsequence)
