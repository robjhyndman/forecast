# h-step in-sample forecasts for time series models.

Returns h-step forecasts for the data used in fitting the model.

## Usage

``` r
# S3 method for class 'ARFIMA'
fitted(object, h = 1, ...)

# S3 method for class 'Arima'
fitted(object, h = 1, ...)

# S3 method for class 'ar'
fitted(object, ...)

# S3 method for class 'bats'
fitted(object, h = 1, ...)

# S3 method for class 'ets'
fitted(object, h = 1, ...)

# S3 method for class 'modelAR'
fitted(object, h = 1, ...)

# S3 method for class 'nnetar'
fitted(object, h = 1, ...)

# S3 method for class 'tbats'
fitted(object, h = 1, ...)
```

## Arguments

- object:

  An object of class `Arima`, `bats`, `tbats`, `ets` or `nnetar`.

- h:

  The number of steps to forecast ahead.

- ...:

  Other arguments.

## Value

A time series of the h-step forecasts.

## See also

[`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md),
[`forecast.bats()`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md),
[`forecast.tbats()`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md),
[`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md),
[`forecast.nnetar()`](https://pkg.robjhyndman.com/forecast/reference/forecast.nnetar.md),
[`residuals.Arima()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md),
[`residuals.bats()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
[`residuals.tbats()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md),
[`residuals.ets()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md),
[`residuals.nnetar()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md).

## Author

Rob J Hyndman & Mitchell O'Hara-Wild

## Examples

``` r
fit <- ets(WWWusage)
plot(WWWusage)
lines(fitted(fit), col = "red")
lines(fitted(fit, h = 2), col = "green")
lines(fitted(fit, h = 3), col = "blue")
legend("topleft", legend = paste("h =", 1:3), col = 2:4, lty = 1)

```
