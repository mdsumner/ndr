# Look up integer index for a coordinate value

Returns the 1-based index of the element nearest to `value`. For
ImplicitCoord, this is O(1) arithmetic. For ExplicitCoord on numeric
values, binary search if sorted, linear scan otherwise.

## Usage

``` r
coord_lookup(x, value)
```

## Arguments

- x:

  A coordinate

- value:

  The value(s) to look up

## Value

Integer index (1-based)
