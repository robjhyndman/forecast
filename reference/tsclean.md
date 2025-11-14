# Identify and replace outliers and missing values in a time series

Uses supsmu for non-seasonal series and a robust STL decomposition for
seasonal series. To estimate missing values and outlier replacements,
linear interpolation is used on the (possibly seasonally adjusted)
series

## Usage

``` r
tsclean(x, replace.missing = TRUE, iterate = 2, lambda = NULL)
```

## Arguments

- x:

  Time series.

- replace.missing:

  If `TRUE`, it not only replaces outliers, but also interpolates
  missing values.

- iterate:

  The number of iterations required.

- lambda:

  Box-Cox transformation parameter. If `lambda = "auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

## Value

Time series

## References

Hyndman (2021) "Detecting time series outliers"
<https://robjhyndman.com/hyndsight/tsoutliers/>.

## See also

[`na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.md),
[`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md),
[`stats::supsmu()`](https://rdrr.io/r/stats/supsmu.html)

## Author

Rob J Hyndman

## Examples

``` r
cleangold <- tsclean(gold)
```
