# Find the common (broadcast) shape for two Variables

Takes two Variables and returns the output dimension names and shape.
Rules:

1.  Output dims = union of input dims (preserving order: left then new
    from right)

2.  For shared dims, sizes must match

3.  For dims present in only one input, the other is treated as size-1

## Usage

``` r
broadcast_shape(a, b)
```

## Arguments

- a, b:

  Variable objects

## Value

A named integer vector: the broadcast shape
