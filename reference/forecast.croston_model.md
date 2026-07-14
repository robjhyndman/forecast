# Forecasts for intermittent demand using Croston's method

Returns forecasts and other information for Croston's forecasts applied
to y.

## Usage

``` r
# S3 method for class 'croston_model'
forecast(object, h = 10, ...)

croston(
  y,
  h = 10,
  alpha = 0.1,
  type = c("croston", "sba", "sbj"),
  x = y,
  opt_alpha = FALSE,
  opt_crit = c("mse", "mae"),
  init = c("naive", "mean")
)
```

## Arguments

- object:

  An object of class `croston_model` as returned by
  [`croston_model()`](https://pkg.robjhyndman.com/forecast/reference/croston_model.md).

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

- ...:

  Additional arguments affecting the forecasts produced. If
  `model = NULL`, `forecast.ts` passes these to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) or
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  depending on the frequency of the time series. If `model` is not
  `NULL`, the arguments are passed to the relevant modelling function.

- y:

  a numeric vector or univariate time series of class `ts`

- alpha:

  Smoothing parameter(s), each between 0 and 1. A single value (the
  default, `0.1`) is shared by the demand and interval SES applications.
  A length-2 vector uses `alpha[1]` for the demand and `alpha[2]` for
  the interval.

- type:

  Which variant of Croston's method to use. Defaults to `"croston"` for
  Croston's method, but can also be set to `"sba"` for the
  Syntetos-Boylan approximation, and `"sbj"` for the
  Shale-Boylan-Johnston method.

- opt_alpha:

  If `TRUE`, optimize the smoothing parameter(s) starting from `alpha`.
  Defaults to `FALSE`, which uses `alpha` directly.

- opt_crit:

  Optimization criterion when `opt_alpha = TRUE`. One of `"mse"` (mean
  squared error) or `"mae"` (mean absolute error).

- init:

  Initial demand and interval values. Either a string method or a
  length-2 numeric `c(demand, interval)`. The `"naive"` method (the
  default) takes the interval from the first interval and `"mean"` from
  the mean interval, both taking demand from the first non-zero value.
  String values are optimized alongside `alpha` when `opt_alpha = TRUE`,
  while numeric values are held fixed.

- x:

  Deprecated. Included for backwards compatibility.

## Value

An object of class `forecast`.

## Details

Based on Croston's (1972) method for intermittent demand forecasting,
also described in Shenstone and Hyndman (2005). Croston's method
involves using simple exponential smoothing (SES) on the non-zero
elements of the time series and a separate application of SES to the
times between non-zero elements of the time series. The smoothing
parameters of the two applications of SES are denoted by `alpha`, and
may be shared (the default) or specified separately for the demand and
interval components.

Note that prediction intervals are not computed as Croston's method has
no underlying stochastic model.

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
forecasts and prediction intervals. The generic accessor functions
`fitted.values` and `residuals` extract various useful features from the
underlying model.

## References

Croston, J. (1972) "Forecasting and stock control for intermittent
demands", *Operational Research Quarterly*, **23**(3), 289-303.

Shale, E.A., Boylan, J.E., & Johnston, F.R. (2006). Forecasting for
intermittent demand: the estimation of an unbiased average. *Journal of
the Operational Research Society*, **57**(5), 588-592.

Shenstone, L., and Hyndman, R.J. (2005) "Stochastic models underlying
Croston's method for intermittent demand forecasting". *Journal of
Forecasting*, **24**, 389-402.

Syntetos A.A., Boylan J.E. (2001). On the bias of intermittent demand
estimates. *International Journal of Production Economics*, **71**,
457–466.

## See also

[`ses()`](https://pkg.robjhyndman.com/forecast/reference/ses.md).

## Author

Rob J Hyndman

## Examples

``` r
y <- rpois(20, lambda = 0.3)
fcast <- croston(y)
autoplot(fcast)

```
