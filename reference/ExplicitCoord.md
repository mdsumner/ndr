# Create an explicit (irregular) coordinate

Create an explicit (irregular) coordinate

## Usage

``` r
ExplicitCoord(dimension = character(0), values = NULL)
```

## Arguments

- dimension:

  Dimension name (character, length 1)

- values:

  Vector of coordinate values

## Examples

``` r
time <- ExplicitCoord(
  dimension = "time",
  values = as.Date("2020-01-01") + 0:364
)
coord_length(time)  # 365
#> [1] 365
```
