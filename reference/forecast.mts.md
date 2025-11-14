# Forecasting time series

`mforecast` is a class of objects for forecasting from multivariate time
series or multivariate time series models. The function invokes
particular *methods* which depend on the class of the first argument.

## Usage

``` r
# S3 method for class 'mts'
forecast(
  object,
  h = if (frequency(object) > 1) 2 * frequency(object) else 10,
  level = c(80, 95),
  fan = FALSE,
  robust = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  find.frequency = FALSE,
  allow.multiplicative.trend = FALSE,
  ...
)
```

## Arguments

- object:

  a multivariate time series or multivariate time series model for which
  forecasts are required

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

- level:

  Confidence levels for prediction intervals.

- fan:

  If `TRUE`, `level` is set to `seq(51, 99, by = 3)`. This is suitable
  for fan plots.

- robust:

  If `TRUE`, the function is robust to missing values and outliers in
  `object`. This argument is only valid when `object` is of class `mts`.

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

- find.frequency:

  If `TRUE`, the function determines the appropriate period, if the data
  is of unknown period.

- allow.multiplicative.trend:

  If `TRUE`, then ETS models with multiplicative trends are allowed.
  Otherwise, only additive or no trend ETS models are permitted.

- ...:

  Additional arguments affecting the forecasts produced.

## Value

An object of class `mforecast`.

The function `summary` is used to obtain and print a summary of the
results, while the function `plot` produces a plot of the multivariate
forecasts and prediction intervals.

The generic accessors functions `fitted.values` and `residuals` extract
various useful features of the value returned by `forecast$model`.

An object of class `mforecast` is a list usually containing at least the
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

  The original time series (either `object` itself or the time series
  used to create the model stored as `object`).

- residuals:

  Residuals from the fitted model. For models with additive errors, the
  residuals will be x minus the fitted values.

- fitted:

  Fitted values (one-step forecasts)

## Details

For example, the function
[`forecast.mlm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mlm.md)
makes multivariate forecasts based on the results produced by
[`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md).

## See also

Other functions which return objects of class `mforecast` are
[`forecast.mlm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mlm.md),
`forecast.varest()`.

## Author

Rob J Hyndman & Mitchell O'Hara-Wild
