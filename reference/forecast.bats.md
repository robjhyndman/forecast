# Forecasting using BATS and TBATS models

Forecasts `h` steps ahead with a BATS model. Prediction intervals are
also produced.

## Usage

``` r
# S3 method for class 'bats'
forecast(object, h, level = c(80, 95), fan = FALSE, biasadj = NULL, ...)

# S3 method for class 'tbats'
forecast(object, h, level = c(80, 95), fan = FALSE, biasadj = NULL, ...)
```

## Arguments

- object:

  An object of class `bats`. Usually the result of a call to
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md).

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

- level:

  Confidence levels for prediction intervals.

- fan:

  If `TRUE`, `level` is set to `seq(51, 99, by = 3)`. This is suitable
  for fan plots.

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

## References

De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011), Forecasting time
series with complex seasonal patterns using exponential smoothing,
*Journal of the American Statistical Association*, **106**(496),
1513-1527.

## See also

[`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md),
[`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md),
[`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md).

## Author

Slava Razbash and Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- bats(USAccDeaths)
plot(forecast(fit))

taylor.fit <- bats(taylor)
plot(forecast(taylor.fit))
} # }
```
