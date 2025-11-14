# Identify and replace outliers in a time series

Uses supsmu for non-seasonal series and a periodic stl decomposition
with seasonal series to identify outliers and estimate their
replacements.

## Usage

``` r
tsoutliers(x, iterate = 2, lambda = NULL)
```

## Arguments

- x:

  Time series.

- iterate:

  The number of iterations required.

- lambda:

  Box-Cox transformation parameter. If `lambda = "auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

## Value

- index:

  Indicating the index of outlier(s)

- replacement:

  Suggested numeric values to replace identified outliers

## References

Hyndman (2021) "Detecting time series outliers"
<https://robjhyndman.com/hyndsight/tsoutliers/>.

## See also

[`na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.md),
[`tsclean()`](https://pkg.robjhyndman.com/forecast/reference/tsclean.md)

## Author

Rob J Hyndman

## Examples

``` r
data(gold)
tsoutliers(gold)
#> $index
#> [1] 770
#> 
#> $replacements
#> [1] 494.9
#> 
```
