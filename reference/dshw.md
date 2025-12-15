# Double-Seasonal Holt-Winters Forecasting

Returns forecasts using Taylor's (2003) Double-Seasonal Holt-Winters
method.

## Usage

``` r
dshw(
  y,
  period1 = NULL,
  period2 = NULL,
  h = 2 * max(period1, period2),
  alpha = NULL,
  beta = NULL,
  gamma = NULL,
  omega = NULL,
  phi = NULL,
  lambda = NULL,
  biasadj = FALSE,
  armethod = TRUE,
  model = NULL
)
```

## Arguments

- y:

  Either an
  [`msts()`](https://pkg.robjhyndman.com/forecast/reference/msts.md)
  object with two seasonal periods or a numeric vector.

- period1:

  Period of the shorter seasonal period. Only used if `y` is not an
  [`msts()`](https://pkg.robjhyndman.com/forecast/reference/msts.md)
  object.

- period2:

  Period of the longer seasonal period. Only used if `y` is not an
  [`msts()`](https://pkg.robjhyndman.com/forecast/reference/msts.md)
  object.

- h:

  Number of periods for forecasting.

- alpha:

  Smoothing parameter for the level. If `NULL`, the parameter is
  estimated using least squares.

- beta:

  Smoothing parameter for the slope. If `NULL`, the parameter is
  estimated using least squares.

- gamma:

  Smoothing parameter for the first seasonal period. If `NULL`, the
  parameter is estimated using least squares.

- omega:

  Smoothing parameter for the second seasonal period. If `NULL`, the
  parameter is estimated using least squares.

- phi:

  Autoregressive parameter. If `NULL`, the parameter is estimated using
  least squares.

- lambda:

  Box-Cox transformation parameter. If `lambda = "auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

- biasadj:

  Use adjusted back-transformed mean for Box-Cox transformations. If
  transformed data is used to produce forecasts and fitted values, a
  regular back transformation will result in median forecasts. If
  biasadj is `TRUE`, an adjustment will be made to produce mean
  forecasts and fitted values.

- armethod:

  If `TRUE`, the forecasts are adjusted using an AR(1) model for the
  errors.

- model:

  If it's specified, an existing model is applied to a new data set.

## Value

An object of class `forecast`.

## Details

Taylor's (2003) double-seasonal Holt-Winters method uses additive trend
and multiplicative seasonality, where there are two seasonal components
which are multiplied together. For example, with a series of half-hourly
data, one would set `period1 = 48` for the daily period and
`period2 = 336` for the weekly period. The smoothing parameter notation
used here is different from that in Taylor (2003); instead it matches
that used in Hyndman et al (2008) and that used for the
[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
function.

## References

Taylor, J.W. (2003) Short-term electricity demand forecasting using
double seasonal exponential smoothing. *Journal of the Operational
Research Society*, **54**, 799-805.

Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D. (2008)
*Forecasting with exponential smoothing: the state space approach*,
Springer-Verlag. <https://robjhyndman.com/expsmooth/>.

## See also

[`stats::HoltWinters()`](https://rdrr.io/r/stats/HoltWinters.html),
[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
fcast <- dshw(taylor)
plot(fcast)

t <- seq(0, 5, by = 1 / 20)
x <- exp(sin(2 * pi * t) + cos(2 * pi * t * 4) + rnorm(length(t), 0, 0.1))
fit <- dshw(x, 20, 5)
plot(fit)
} # }
```
