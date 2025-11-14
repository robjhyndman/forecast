# Forecasting using ETS models

Returns forecasts and other information for univariate ETS models.

## Usage

``` r
# S3 method for class 'ets'
forecast(
  object,
  h = if (object$m > 1) 2 * object$m else 10,
  level = c(80, 95),
  fan = FALSE,
  simulate = FALSE,
  bootstrap = FALSE,
  npaths = 5000,
  PI = TRUE,
  lambda = object$lambda,
  biasadj = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `ets`. Usually the result of a call to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

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
  resampled errors (rather than normally distributed errors).

- npaths:

  Number of sample paths used in computing simulated prediction
  intervals.

- PI:

  If `TRUE`, prediction intervals are produced, otherwise only point
  forecasts are calculated. If `PI` is `FALSE`, then `level`, `fan`,
  `simulate`, `bootstrap` and `npaths` are all ignored.

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

## Value

An object of class `forecast`.

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

[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md),
[`ses()`](https://pkg.robjhyndman.com/forecast/reference/ses.md),
[`holt()`](https://pkg.robjhyndman.com/forecast/reference/ses.md),
[`hw()`](https://pkg.robjhyndman.com/forecast/reference/ses.md).

## Author

Rob J Hyndman

## Examples

``` r
fit <- ets(USAccDeaths)
plot(forecast(fit, h = 48))

```
