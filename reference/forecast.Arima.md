# Forecasting using ARIMA or ARFIMA models

Returns forecasts and other information for univariate ARIMA models.

## Usage

``` r
# S3 method for class 'fracdiff'
forecast(
  object,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  simulate = FALSE,
  bootstrap = FALSE,
  innov = NULL,
  npaths = 5000,
  lambda = object$lambda,
  biasadj = attr(lambda, "biasadj"),
  ...
)

# S3 method for class 'Arima'
forecast(
  object,
  h = if (object$arma[5] > 1) 2 * object$arma[5] else 10,
  level = c(80, 95),
  fan = FALSE,
  xreg = NULL,
  simulate = FALSE,
  bootstrap = FALSE,
  innov = NULL,
  npaths = 5000,
  lambda = object$lambda,
  biasadj = attr(lambda, "biasadj"),
  ...
)

# S3 method for class 'ar'
forecast(
  object,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  simulate = FALSE,
  bootstrap = FALSE,
  innov = NULL,
  npaths = 5000,
  lambda = NULL,
  biasadj = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class `Arima`, `ar` or `fracdiff`. Usually the result of
  a call to [`stats::arima()`](https://rdrr.io/r/stats/arima.html),
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
  [`stats::ar()`](https://rdrr.io/r/stats/ar.html),
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  or
  [`fracdiff::fracdiff()`](https://rdrr.io/pkg/fracdiff/man/fracdiff.html).

- h:

  Number of periods for forecasting. If `xreg` is used, `h` is ignored
  and the number of forecast periods is set to the number of rows of
  `xreg`.

- level:

  Confidence levels for prediction intervals.

- fan:

  If `TRUE`, `level` is set to `seq(51, 99, by = 3)`. This is suitable
  for fan plots.

- simulate:

  If `TRUE`, prediction intervals are produced by simulation rather than
  using analytic formulae. Errors are assumed to be normally
  distributed.

- bootstrap:

  If `TRUE`, then prediction intervals are produced by simulation using
  resampled errors (rather than normally distributed errors). Ignored if
  `innov` is not `NULL`.

- innov:

  Optional matrix of future innovations to be used in simulations.
  Ignored if `simulate = FALSE`. If provided, this overrides the
  `bootstrap` argument. The matrix should have `h` rows and `npaths`
  columns.

- npaths:

  Number of sample paths used in computing simulated prediction
  intervals.

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

- ...:

  Other arguments are ignored.

- xreg:

  Future values of any regression variables. A numerical vector or
  matrix of external regressors; it should not be a data frame.

## Value

An object of class `forecast`.

## Details

For `Arima` or `ar` objects, the function calls
[`stats::predict.Arima()`](https://rdrr.io/r/stats/predict.arima.html)
or [stats::predict.ar](https://rdrr.io/r/stats/ar.html) and constructs
an object of class `forecast` from the results. For `fracdiff` objects,
the calculations are all done within
[`fracdiff::fracdiff()`](https://rdrr.io/pkg/fracdiff/man/fracdiff.html)
using the equations given by Peiris and Perera (1988).

## forecast class

An object of class `forecast` is a list usually containing at least the
following elements:

- model:

  A list containing information about the fitted model

- method:

  The name of the forecasting method as a character string

- mean:

  Point forecasts as a time series

- lower:

  Lower limits for prediction intervals

- upper:

  Upper limits for prediction intervals

- level:

  The confidence values associated with the prediction intervals

- x:

  The original time series.

- residuals:

  Residuals from the fitted model. For models with additive errors, the
  residuals will be x minus the fitted values.

- fitted:

  Fitted values (one-step forecasts)

The function `summary` can be used to obtain and print a summary of the
results, while the functions `plot` and `autoplot` produce plots of the
forecasts and prediction intervals. The generic accessors functions
`fitted.values` and `residuals` extract various useful features from the
underlying model.

## References

Peiris, M. & Perera, B. (1988), On prediction with fractionally
differenced ARIMA models, *Journal of Time Series Analysis*, **9**(3),
215-220.

## See also

[`stats::predict.Arima()`](https://rdrr.io/r/stats/predict.arima.html),
[`stats::predict.ar()`](https://rdrr.io/r/stats/ar.html),
[`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md),
[`stats::arima()`](https://rdrr.io/r/stats/arima.html),
[`stats::ar()`](https://rdrr.io/r/stats/ar.html),
[`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md).

## Author

Rob J Hyndman

## Examples

``` r
fit <- Arima(WWWusage, c(3, 1, 0))
plot(forecast(fit))


library(fracdiff)
x <- fracdiff.sim(100, ma = -0.4, d = 0.3)$series
fit <- arfima(x)
plot(forecast(fit, h = 30))

```
