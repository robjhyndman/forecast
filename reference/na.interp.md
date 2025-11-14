# Interpolate missing values in a time series

By default, uses linear interpolation for non-seasonal series. For
seasonal series, a robust STL decomposition is first computed. Then a
linear interpolation is applied to the seasonally adjusted data, and the
seasonal component is added back.

## Usage

``` r
na.interp(
  x,
  lambda = NULL,
  linear = (frequency(x) <= 1 || sum(!is.na(x)) <= 2 * frequency(x))
)
```

## Arguments

- x:

  Time series.

- lambda:

  Box-Cox transformation parameter. If `lambda = "auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

- linear:

  Should a linear interpolation be used.

## Value

Time series

## Details

A more general and flexible approach is available using `na.approx` in
the [zoo](https://CRAN.R-project.org/package=zoo) package.

## See also

[`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md)

## Author

Rob J Hyndman

## Examples

``` r
data(gold)
plot(na.interp(gold))

```
