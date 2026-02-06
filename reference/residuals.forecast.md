# Residuals for various time series models

Returns time series of residuals from a fitted model.

## Usage

``` r
# S3 method for class 'forecast'
residuals(object, type = c("innovation", "response"), ...)

# S3 method for class 'ar'
residuals(object, type = c("innovation", "response"), ...)

# S3 method for class 'Arima'
residuals(object, type = c("innovation", "response", "regression"), h = 1, ...)

# S3 method for class 'bats'
residuals(object, type = c("innovation", "response"), h = 1, ...)

# S3 method for class 'tbats'
residuals(object, type = c("innovation", "response"), h = 1, ...)

# S3 method for class 'ets'
residuals(object, type = c("innovation", "response"), h = 1, ...)

# S3 method for class 'ARFIMA'
residuals(object, type = c("innovation", "response"), ...)

# S3 method for class 'nnetar'
residuals(object, type = c("innovation", "response"), h = 1, ...)

# S3 method for class 'stlm'
residuals(object, type = c("innovation", "response"), ...)

# S3 method for class 'tslm'
residuals(
  object,
  type = c("innovation", "response", "working", "deviance"),
  ...
)
```

## Arguments

- object:

  An object containing a time series model of class `ar`, `Arima`,
  `bats`, `ets`, `arfima`, `nnetar`, `stlm` or `tslm`. If `object` is of
  class `forecast`, then the function will return `object$residuals` if
  it exists, otherwise it returns the differences between the
  observations and their fitted values.

- type:

  Type of residual. The `innovation` residuals are estimates of the
  innovations in the model; these should look like white noise for a
  well-fitted model. The `response` residuals are equal to the
  observation minus its fitted value. For many models, `innovation` and
  `response` residuals will be identical, but not if there has been a
  transformation used, or for an ETS model with multiplicative errors. A
  `regression` residual from a dynamic regression model
  (`residuals.Arima`) is equal to the response variable minus the linear
  combination of predictors.

- ...:

  Other arguments not used.

- h:

  If `type = "response"`, then the fitted values are computed for
  `h`-step forecasts.

## Value

A `ts` object.

## Details

Innovation residuals correspond to the white noise process that drives
the evolution of the time series model. Response residuals are the
difference between the observations and the fitted values (equivalent to
`h`-step forecasts). For functions with no `h` argument, `h = 1`. For
homoscedastic models, the innovation residuals and the response
residuals for `h = 1` are identical. Regression residuals are available
for regression models with ARIMA errors, and are equal to the original
data minus the effect of the regression variables. If there are no
regression variables, the errors will be identical to the original
series (possibly adjusted to have zero mean). `arima.errors` is a
deprecated function which is identical to
`residuals.Arima(object, type="regression")`. For `nnetar` objects, when
`type = "innovations"` and `lambda` is used, a matrix of time-series
consisting of the residuals from each of the fitted neural networks is
returned.

## See also

[`fitted.Arima()`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md),
[`checkresiduals()`](https://pkg.robjhyndman.com/forecast/reference/checkresiduals.md).

## Author

Rob J Hyndman

## Examples

``` r
fit <- Arima(lynx, order = c(4, 0, 0), lambda = 0.5)

plot(residuals(fit))

plot(residuals(fit, type = "response"))
```
