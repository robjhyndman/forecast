# Forecasting using a bagged model

The bagged model forecasting method.

## Usage

``` r
baggedModel(y, bootstrapped_series = bld.mbb.bootstrap(y, 100), fn = ets, ...)

baggedETS(y, bootstrapped_series = bld.mbb.bootstrap(y, 100), ...)
```

## Arguments

- y:

  A numeric vector or univariate time series of class `ts`.

- bootstrapped_series:

  bootstrapped versions of y.

- fn:

  the forecast function to use. Default is
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).

- ...:

  Other arguments passed to the forecast function.

## Value

Returns an object of class `baggedModel`.

The function `print` is used to obtain and print a summary of the
results.

- models:

  A list containing the fitted ensemble models.

- method:

  The function for producing a forecastable model.

- y:

  The original time series.

- bootstrapped_series:

  The bootstrapped series.

- modelargs:

  The arguments passed through to `fn`.

- fitted:

  Fitted values (one-step forecasts). The mean of the fitted values is
  calculated over the ensemble.

- residuals:

  Original values minus fitted values.

## Details

This function implements the bagged model forecasting method described
in Bergmeir et al. By default, the
[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
function is applied to all bootstrapped series. Base models other than
[`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) can be
given by the parameter `fn`. Using the default parameters, the function
[`bld.mbb.bootstrap()`](https://pkg.robjhyndman.com/forecast/reference/bld.mbb.bootstrap.md)
is used to calculate the bootstrapped series with the Box-Cox and
Loess-based decomposition (BLD) bootstrap. The function
[`forecast.baggedModel()`](https://pkg.robjhyndman.com/forecast/reference/forecast.baggedModel.md)
can then be used to calculate forecasts.

`baggedETS` is a wrapper for `baggedModel`, setting `fn` to "ets". This
function is included for backwards compatibility only, and may be
deprecated in the future.

## References

Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging
Exponential Smoothing Methods using STL Decomposition and Box-Cox
Transformation. International Journal of Forecasting 32, 303-312.

## Author

Christoph Bergmeir, Fotios Petropoulos

## Examples

``` r
fit <- baggedModel(WWWusage)
fcast <- forecast(fit)
plot(fcast)

```
