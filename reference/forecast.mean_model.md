# Mean Forecast

Returns forecasts and prediction intervals for a Gaussian iid model.
`meanf()` is a convenience function that combines
[`mean_model()`](https://pkg.robjhyndman.com/forecast/reference/mean_model.md)
and [`forecast()`](https://generics.r-lib.org/reference/forecast.html).

## Usage

``` r
# S3 method for class 'mean_model'
forecast(
  object,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = NULL,
  bootstrap = FALSE,
  npaths = 5000,
  ...
)

meanf(
  y,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  bootstrap = FALSE,
  npaths = 5000,
  x = y
)
```

## Arguments

- object:

  An object of class `mean_model` as returned by
  [`mean_model()`](https://pkg.robjhyndman.com/forecast/reference/mean_model.md).

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

- level:

  Confidence levels for prediction intervals.

- fan:

  If `TRUE`, `level` is set to `seq(51, 99, by = 3)`. This is suitable
  for fan plots.

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

- bootstrap:

  If `TRUE`, then prediction intervals are produced by simulation using
  resampled errors (rather than normally distributed errors).

- npaths:

  Number of sample paths used in computing simulated prediction
  intervals.

- ...:

  Additional arguments not used.

- y:

  a numeric vector or univariate time series of class `ts`

- x:

  Deprecated. Included for backwards compatibility.

## Details

The model assumes that the data are independent and identically
distributed

\$\$Y_t \sim N(\mu,\sigma^2)\$\$

Forecasts are given by

\$\$Y\_{n+h\|n}=\mu\$\$

where \\\mu\\ is estimated by the sample mean.

The function [`summary()`](https://rdrr.io/r/base/summary.html) is used
to obtain and print a summary of the results, while the function
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) produces a plot
of the forecasts and prediction intervals. The generic accessor
functions
[`stats::fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
[`stats::residuals()`](https://rdrr.io/r/stats/residuals.html) extract
useful features of the object returned by
[`mean_model()`](https://pkg.robjhyndman.com/forecast/reference/mean_model.md).

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

## See also

[`mean_model()`](https://pkg.robjhyndman.com/forecast/reference/mean_model.md)

## Author

Rob J Hyndman

## Examples

``` r
fit_nile <- mean_model(Nile)
fit_nile |> forecast(h = 10) |> autoplot()

nile.fcast <- meanf(Nile, h = 10)
```
