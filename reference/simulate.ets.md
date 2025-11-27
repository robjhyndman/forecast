# Simulation from a time series model

Returns a time series based on the model object `object`.

## Usage

``` r
# S3 method for class 'ets'
simulate(
  object,
  nsim = length(object$x),
  seed = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  ...
)

# S3 method for class 'Arima'
simulate(
  object,
  nsim = length(object$x),
  seed = NULL,
  xreg = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  lambda = object$lambda,
  ...
)

# S3 method for class 'ar'
simulate(
  object,
  nsim = object$n.used,
  seed = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  ...
)

# S3 method for class 'rw_model'
simulate(
  object,
  nsim = length(object$x),
  seed = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  lambda = object$lambda,
  ...
)

# S3 method for class 'fracdiff'
simulate(
  object,
  nsim = object$n,
  seed = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  lambda = object$lambda,
  ...
)

# S3 method for class 'nnetar'
simulate(
  object,
  nsim = length(object$x),
  seed = NULL,
  xreg = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  lambda = object$lambda,
  ...
)

# S3 method for class 'modelAR'
simulate(
  object,
  nsim = length(object$x),
  seed = NULL,
  xreg = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  lambda = object$lambda,
  ...
)

# S3 method for class 'tbats'
simulate(
  object,
  nsim = length(object$y),
  seed = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  ...
)

# S3 method for class 'spline_model'
simulate(
  object,
  nsim = length(object$y),
  seed = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  lambda = object$lambda,
  ...
)
```

## Arguments

- object:

  An object representing a fitted time series model. For example, it may
  be of class `ets`, `Arima`, `ar`, `nnetar`, etc.

- nsim:

  Number of periods for the simulated series. Ignored if either `xreg`
  or `innov` are not `NULL`. Otherwise the default is the length of
  series used to train model (or 100 if no data found).

- seed:

  Either `NULL` or an integer that will be used in a call to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before simulating
  the time series. The default, `NULL`, will not change the random
  generator state.

- future:

  Produce sample paths that are future to and conditional on the data in
  `object`. Otherwise simulate unconditionally.

- bootstrap:

  Do simulation using resampled errors rather than normally distributed
  errors or errors provided as `innov`.

- innov:

  A vector of innovations to use as the error series. Ignored if
  `bootstrap = TRUE`. If not `NULL`, the value of `nsim` is set to
  length of `innov`.

- ...:

  Other arguments, not currently used.

- xreg:

  New values of `xreg` to be used for forecasting. The value of `nsim`
  is set to the number of rows of `xreg` if it is not `NULL`.

- lambda:

  Box-Cox transformation parameter. If `lambda = "auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

## Value

An object of class `ts`.

## Details

With `simulate.Arima()`, the `object` should be produced by
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md) or
[`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
rather than [`stats::arima()`](https://rdrr.io/r/stats/arima.html). By
default, the error series is assumed normally distributed and generated
using [`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html). If
`innov` is present, it is used instead. If `bootstrap = TRUE` and
`innov = NULL`, the residuals are resampled instead.

When `future = TRUE`, the sample paths are conditional on the data. When
`future = FALSE` and the model is stationary, the sample paths do not
depend on the data at all. When `future = FALSE` and the model is
non-stationary, the location of the sample paths is arbitrary, so they
all start at the value of the first observation.

## See also

[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md),
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md),
[`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
[`ar()`](https://rdrr.io/r/stats/ar.html),
[`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md),
[`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md).

## Author

Rob J Hyndman

## Examples

``` r
fit <- ets(USAccDeaths)
plot(USAccDeaths, xlim = c(1973, 1982))
lines(simulate(fit, 36), col = "red")
```
