# Extract a DataArray from a Dataset

Use `ds$temperature` or `ds[["temperature"]]` to extract a variable as a
DataArray with its relevant coordinates attached.

## Usage

``` r
# S3 method for class '`ndr::Dataset`'
ds$var_name
```

## Arguments

- ds:

  A Dataset

- var_name:

  Character name of the variable

## Value

A DataArray
