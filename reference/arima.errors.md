# Errors from a regression model with ARIMA errors

Returns time series of the regression residuals from a fitted ARIMA
model.

## Usage

``` r
arima.errors(object)
```

## Arguments

- object:

  An object containing a time series model of class `Arima`.

## Value

A `ts` object

## Details

This is a deprecated function which is identical to
[`residuals.Arima(object, type="regression")`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
Regression residuals are equal to the original data minus the effect of
any regression variables. If there are no regression variables, the
errors will be identical to the original series (possibly adjusted to
have zero mean).

## See also

[`residuals.Arima()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md).

## Author

Rob J Hyndman
