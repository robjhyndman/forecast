# Forecasting model using STL with a generative time series model

Forecasts of STL objects are obtained by applying a non-seasonal
forecasting model to the seasonally adjusted data and re-seasonalizing
using the last year of the seasonal component. `stlm` takes a time
series `y`, applies an STL decomposition, and models the seasonally
adjusted data using the model passed as `modelfunction` or specified
using `method`. It returns an object that includes the original STL
decomposition and a time series model fitted to the seasonally adjusted
data. This object can be passed to the `forecast.stlm` for forecasting.

## Usage

``` r
stlm(
  y,
  s.window = 7 + 4 * seq(6),
  t.window = NULL,
  robust = FALSE,
  method = c("ets", "arima"),
  modelfunction = NULL,
  model = NULL,
  etsmodel = "ZZN",
  lambda = NULL,
  biasadj = FALSE,
  xreg = NULL,
  allow.multiplicative.trend = FALSE,
  x = y,
  ...
)
```

## Arguments

- y:

  a numeric vector or univariate time series of class `ts`

- s.window:

  Either the character string `"periodic"` or the span (in lags) of the
  loess window for seasonal extraction.

- t.window:

  A number to control the smoothness of the trend. See
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html) for details.

- robust:

  If `TRUE`, robust fitting will used in the loess procedure within
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html).

- method:

  Method to use for forecasting the seasonally adjusted series.

- modelfunction:

  An alternative way of specifying the function for modelling the
  seasonally adjusted series. If `modelfunction` is not `NULL`, then
  `method` is ignored. Otherwise `method` is used to specify the time
  series model to be used.

- model:

  Output from a previous call to `stlm`. If a `stlm` model is passed,
  this same model is fitted to y without re-estimating any parameters.

- etsmodel:

  The ets model specification passed to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md). By
  default it allows any non-seasonal model. If `method != "ets"`, this
  argument is ignored.

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

- xreg:

  Historical regressors to be used in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  when `method = "arima"`.

- allow.multiplicative.trend:

  If `TRUE`, then ETS models with multiplicative trends are allowed.
  Otherwise, only additive or no trend ETS models are permitted.

- x:

  Deprecated. Included for backwards compatibility.

- ...:

  Other arguments passed to `modelfunction`.

## Value

An object of class `stlm`.

## Details

The time series model for the seasonally adjusted data can be specified
in `stlm` using either `method` or `modelfunction`. The `method`
argument provides a shorthand way of specifying `modelfunction` for a
few special cases. More generally, `modelfunction` can be any function
with first argument a `ts` object, that returns an object that can be
passed to
[`forecast()`](https://generics.r-lib.org/reference/forecast.html). For
example, `modelfunction = ar` uses the
[`ar()`](https://rdrr.io/r/stats/ar.html) function for modelling the
seasonally adjusted series.

## See also

[`stats::stl()`](https://rdrr.io/r/stats/stl.html),
[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md),
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md).

## Author

Rob J Hyndman

## Examples

``` r
tsmod <- stlm(USAccDeaths, modelfunction = ar)
forecast(tsmod, h = 36) |> autoplot()


decomp <- stl(USAccDeaths, s.window = "periodic")
forecast(decomp) |> autoplot()
```
