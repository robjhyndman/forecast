# Exponential smoothing forecasts

Returns forecasts and other information for exponential smoothing
forecasts applied to `y`.

## Usage

``` r
ses(
  y,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  initial = c("optimal", "simple"),
  alpha = NULL,
  lambda = NULL,
  biasadj = FALSE,
  x = y,
  ...
)

holt(
  y,
  h = 10,
  damped = FALSE,
  level = c(80, 95),
  fan = FALSE,
  initial = c("optimal", "simple"),
  exponential = FALSE,
  alpha = NULL,
  beta = NULL,
  phi = NULL,
  lambda = NULL,
  biasadj = FALSE,
  x = y,
  ...
)

hw(
  y,
  h = 2 * frequency(x),
  seasonal = c("additive", "multiplicative"),
  damped = FALSE,
  level = c(80, 95),
  fan = FALSE,
  initial = c("optimal", "simple"),
  exponential = FALSE,
  alpha = NULL,
  beta = NULL,
  gamma = NULL,
  phi = NULL,
  lambda = NULL,
  biasadj = FALSE,
  x = y,
  ...
)
```

## Arguments

- y:

  a numeric vector or univariate time series of class `ts`

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

- level:

  Confidence levels for prediction intervals.

- fan:

  If `TRUE`, `level` is set to `seq(51, 99, by = 3)`. This is suitable
  for fan plots.

- initial:

  Method used for selecting initial state values. If `optimal`, the
  initial values are optimized along with the smoothing parameters using
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md). If
  `simple`, the initial values are set to values obtained using simple
  calculations on the first few observations. See Hyndman &
  Athanasopoulos (2014) for details.

- alpha:

  Value of smoothing parameter for the level. If `NULL`, it will be
  estimated.

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

- x:

  Deprecated. Included for backwards compatibility.

- ...:

  Other arguments passed to `forecast.ets`.

- damped:

  If `TRUE`, use a damped trend.

- exponential:

  If `TRUE`, an exponential trend is fitted. Otherwise, the trend is
  (locally) linear.

- beta:

  Value of smoothing parameter for the trend. If `NULL`, it will be
  estimated.

- phi:

  Value of damping parameter if `damped = TRUE`. If `NULL`, it will be
  estimated.

- seasonal:

  Type of seasonality in `hw` model. `"additive"` or `"multiplicative"`.

- gamma:

  Value of smoothing parameter for the seasonal component. If `NULL`, it
  will be estimated.

## Value

An object of class `forecast`.

## Details

ses, holt and hw are simply convenient wrapper functions for
`forecast(ets(...))`.

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

Hyndman, R.J., Koehler, A.B., Ord, J.K., Snyder, R.D. (2008)
*Forecasting with exponential smoothing: the state space approach*,
Springer-Verlag: New York. <https://robjhyndman.com/expsmooth/>.

Hyndman and Athanasopoulos (2018) *Forecasting: principles and
practice*, 2nd edition, OTexts: Melbourne, Australia.
<https://otexts.com/fpp2/>

## See also

[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md),
[`stats::HoltWinters()`](https://rdrr.io/r/stats/HoltWinters.html),
[`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
[`stats::arima()`](https://rdrr.io/r/stats/arima.html).

## Author

Rob J Hyndman

## Examples

``` r
fcast <- holt(airmiles)
plot(fcast)

deaths.fcast <- hw(USAccDeaths, h = 48)
plot(deaths.fcast)

```
