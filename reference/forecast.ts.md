# Forecasting time series

`forecast` is a generic function for forecasting from time series or
time series models. The function invokes particular *methods* which
depend on the class of the first argument.

## Usage

``` r
# S3 method for class 'ts'
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
  model = NULL,
  ...
)

# Default S3 method
forecast(object, ...)

# S3 method for class 'forecast'
print(x, ...)
```

## Arguments

- object:

  a time series or time series model for which forecasts are required.

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
  `object`. This argument is only valid when `object` is of class `ts`.

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

- model:

  An object describing a time series model; e.g., one of of class `ets`,
  `Arima`, `bats`, `bats`, or `nnetar`.

- ...:

  Additional arguments affecting the forecasts produced. If
  `model = NULL`, `forecast.ts` passes these to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) or
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  depending on the frequency of the time series. If `model` is not
  `NULL`, the arguments are passed to the relevant modelling function.

- x:

  a numeric vector or time series of class `ts`.

## Value

An object of class `forecast`.

## Details

For example, the function
[`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
makes forecasts based on the results produced by
[`stats::arima()`](https://rdrr.io/r/stats/arima.html).

If `model = NULL`,the function `forecast.ts()` makes forecasts using
[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) models
(if the data are non-seasonal or the seasonal period is 12 or less) or
[`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
(if the seasonal period is 13 or more).

If `model` is not `NULL`, `forecast.ts` will apply the `model` to the
`object` time series, and then generate forecasts accordingly.

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

Other functions which return objects of class `forecast` are
[`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md),
[`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md),
[`forecast.HoltWinters()`](https://pkg.robjhyndman.com/forecast/reference/forecast.HoltWinters.md),
[`forecast.StructTS()`](https://pkg.robjhyndman.com/forecast/reference/forecast.StructTS.md),
[`meanf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md),
[`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
[`splinef()`](https://pkg.robjhyndman.com/forecast/reference/splinef.md),
[`thetaf()`](https://pkg.robjhyndman.com/forecast/reference/thetaf.md),
[`croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md),
[`ses()`](https://pkg.robjhyndman.com/forecast/reference/ses.md),
[`holt()`](https://pkg.robjhyndman.com/forecast/reference/ses.md),
[`hw()`](https://pkg.robjhyndman.com/forecast/reference/ses.md).

## Author

Rob J Hyndman

## Examples

``` r
WWWusage |> forecast() |> plot()

fit <- ets(window(WWWusage, end = 60))
fc <- forecast(WWWusage, model = fit)
#> Model is being refit with current smoothing parameters but initial states are being re-estimated.
#> Set 'use.initial.values=TRUE' if you want to re-use existing initial values.
```
