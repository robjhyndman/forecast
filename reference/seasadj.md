# Seasonal adjustment

Returns seasonally adjusted data constructed by removing the seasonal
component.

## Usage

``` r
seasadj(object, ...)

# S3 method for class 'stl'
seasadj(object, ...)

# S3 method for class 'mstl'
seasadj(object, ...)

# S3 method for class 'decomposed.ts'
seasadj(object, ...)

# S3 method for class 'tbats'
seasadj(object, ...)

# S3 method for class 'seas'
seasadj(object, ...)
```

## Arguments

- object:

  Object created by
  [`stats::decompose()`](https://rdrr.io/r/stats/decompose.html),
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html) or
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md).

- ...:

  Other arguments not currently used.

## Value

Univariate time series.

## See also

[`stats::stl()`](https://rdrr.io/r/stats/stl.html),
[`stats::decompose()`](https://rdrr.io/r/stats/decompose.html),
[`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md).

## Author

Rob J Hyndman

## Examples

``` r
plot(AirPassengers)
lines(seasadj(decompose(AirPassengers, "multiplicative")), col = 4)

```
