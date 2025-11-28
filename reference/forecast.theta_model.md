# Theta method forecasts.

Returns forecasts and prediction intervals for a theta method forecast.
`thetaf()` is a convenience function that combines
[`theta_model()`](https://pkg.robjhyndman.com/forecast/reference/theta_model.md)
and `forecast.theta_model()`. The theta method of Assimakopoulos and
Nikolopoulos (2000) is equivalent to simple exponential smoothing with
drift (Hyndman and Billah, 2003). The series is tested for seasonality
using the test outlined in A&N. If deemed seasonal, the series is
seasonally adjusted using a classical multiplicative decomposition
before applying the theta method. The resulting forecasts are then
reseasonalized. Prediction intervals are computed using the underlying
state space model.

## Usage

``` r
# S3 method for class 'theta_model'
forecast(
  object,
  h = if (frequency(object$y) > 1) 2 * frequency(object$y) else 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = FALSE,
  ...
)

thetaf(
  y,
  h = if (frequency(y) > 1) 2 * frequency(y) else 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  x = y,
  ...
)
```

## Arguments

- object:

  An object of class `theta_model` created by
  [`theta_model()`](https://pkg.robjhyndman.com/forecast/reference/theta_model.md).

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

- ...:

  Other arguments passed to `forecast.ets`.

- y:

  a numeric vector or univariate time series of class `ts`

- x:

  Deprecated. Included for backwards compatibility.

## Value

An object of class `forecast`.

## Details

More general theta methods are available in the
[forecTheta](https://CRAN.R-project.org/package=forecTheta) package.

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

Assimakopoulos, V. and Nikolopoulos, K. (2000). The theta model: a
decomposition approach to forecasting. *International Journal of
Forecasting* **16**, 521-530.

Hyndman, R.J., and Billah, B. (2003) Unmasking the Theta method.
*International J. Forecasting*, **19**, 287-290.

## See also

[`stats::arima()`](https://rdrr.io/r/stats/arima.html),
[`meanf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md),
[`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
[`ses()`](https://pkg.robjhyndman.com/forecast/reference/ses.md)

## Author

Rob J Hyndman

## Examples

``` r
nile_fit <- theta_model(Nile)
forecast(nile_fit) |> autoplot()
```
