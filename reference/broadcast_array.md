# Broadcast an array from one shape to another

Expands size-1 dimensions by repeating values. Both shapes must have the
same length and names (use align_data first).

## Usage

``` r
broadcast_array(data, from_shape, to_shape)
```

## Arguments

- data:

  An R array

- from_shape:

  Named integer vector (current shape, may contain 1s)

- to_shape:

  Named integer vector (target shape)

## Value

An R array with dim = to_shape
