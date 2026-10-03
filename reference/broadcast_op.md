# Broadcast two Variables to a common shape and apply an operation

This is the workhorse behind arithmetic on Variables. Handles dimension
alignment, shape checking, broadcasting, and returns a new Variable.

## Usage

``` r
broadcast_op(a, b, op)
```

## Arguments

- a, b:

  Variable objects (or scalars)

- op:

  A binary function (e.g. `+`, `*`)

## Value

A new Variable with broadcast dimensions
