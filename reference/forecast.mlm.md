# Forecast a multiple linear model with possible time series components

`forecast.mlm` is used to predict multiple linear models, especially
those involving trend and seasonality components.

## Usage

``` r
# S3 method for class 'mlm'
forecast(
  object,
  newdata,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = attr(object$lambda, "biasadj"),
  ts = TRUE,
  ...
)
```

## Arguments

- object:

  Object of class "mlm", usually the result of a call to
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md).

- newdata:

  An optional data frame in which to look for variables with which to
  predict. If omitted, it is assumed that the only variables are trend
  and season, and `h` forecasts are produced.

- h:

  Number of periods for forecasting. Ignored if `newdata` present.

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

- ts:

  If `TRUE`, the forecasts will be treated as time series provided the
  original data is a time series; the `newdata` will be interpreted as
  related to the subsequent time periods. If `FALSE`, any time series
  attributes of the original data will be ignored.

- ...:

  Other arguments passed to
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md).

## Value

An object of class `mforecast`.

The function `summary` is used to obtain and print a summary of the
results, while the function `plot` produces a plot of the forecasts and
prediction intervals.

The generic accessor functions `fitted.values` and `residuals` extract
useful features of the value returned by `forecast.lm`.

An object of class `mforecast` is a list containing at least the
following elements:

- model:

  A list containing information about the fitted model

- method:

  The name of the forecasting method as a character string

- mean:

  Point forecasts as a multivariate time series

- lower:

  Lower limits for prediction intervals of each series

- upper:

  Upper limits for prediction intervals of each series

- level:

  The confidence values associated with the prediction intervals

- x:

  The historical data for the response variable.

- residuals:

  Residuals from the fitted model. That is x minus fitted values.

- fitted:

  Fitted values

## Details

`forecast.mlm` is largely a wrapper for
[`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
except that it allows forecasts to be generated on multiple series.
Also, the output is reformatted into a `mforecast` object.

## See also

[`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md),
[`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md),
[`stats::lm()`](https://rdrr.io/r/stats/lm.html).

## Author

Mitchell O'Hara-Wild

## Examples

``` r
lungDeaths <- cbind(mdeaths, fdeaths)
fit <- tslm(lungDeaths ~ trend + season)
fcast <- forecast(fit, h = 10)

carPower <- as.matrix(mtcars[, c("qsec", "hp")])
carmpg <- mtcars[, "mpg"]
fit <- lm(carPower ~ carmpg)
fcast <- forecast(fit, newdata = data.frame(carmpg = 30))
```
