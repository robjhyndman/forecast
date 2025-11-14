# Forecasting using a bagged model

Returns forecasts and other information for bagged models.

## Usage

``` r
# S3 method for class 'baggedModel'
forecast(
  object,
  h = if (frequency(object$y) > 1) 2 * frequency(object$y) else 10,
  ...
)
```

## Arguments

- object:

  An object of class `baggedModel` resulting from a call to
  [`baggedModel()`](https://pkg.robjhyndman.com/forecast/reference/baggedModel.md).

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

- ...:

  Other arguments, passed on to the
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  function of the original method

## Value

An object of class `forecast`.

## Details

Intervals are calculated as min and max values over the point forecasts
from the models in the ensemble. I.e., the intervals are not prediction
intervals, but give an indication of how different the forecasts within
the ensemble are.

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

Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging
Exponential Smoothing Methods using STL Decomposition and Box-Cox
Transformation. International Journal of Forecasting 32, 303-312.

## See also

[`baggedModel()`](https://pkg.robjhyndman.com/forecast/reference/baggedModel.md).

## Author

Christoph Bergmeir, Fotios Petropoulos

## Examples

``` r
fit <- baggedModel(WWWusage)
fcast <- forecast(fit)
plot(fcast)


if (FALSE) { # \dontrun{
fit2 <- baggedModel(WWWusage, fn = "auto.arima")
fcast2 <- forecast(fit2)
plot(fcast2)
accuracy(fcast2)
} # }
```
