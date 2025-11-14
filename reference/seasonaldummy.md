# Seasonal dummy variables

`seasonaldummy` returns a matrix of dummy variables suitable for use in
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md),
[`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
or [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md).
The last season is omitted and used as the control.

## Usage

``` r
seasonaldummy(x, h = NULL)

seasonaldummyf(x, h)
```

## Arguments

- x:

  Seasonal time series: a `ts` or a `msts` object

- h:

  Number of periods ahead to forecast (optional)

## Value

Numerical matrix.

## Details

`seasonaldummyf` is deprecated, instead use the `h` argument in
`seasonaldummy`.

The number of dummy variables is determined from the time series
characteristics of `x`. When `h` is missing, the length of `x` also
determines the number of rows for the matrix returned by
`seasonaldummy`. the value of `h` determines the number of rows for the
matrix returned by `seasonaldummy`, typically used for forecasting. The
values within `x` are not used.

## See also

[`fourier()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)

## Author

Rob J Hyndman

## Examples

``` r
plot(ldeaths)


# Using seasonal dummy variables
month <- seasonaldummy(ldeaths)
deaths.lm <- tslm(ldeaths ~ month)
tsdisplay(residuals(deaths.lm))

ldeaths.fcast <- forecast(
  deaths.lm,
  data.frame(month = I(seasonaldummy(ldeaths, 36)))
)
plot(ldeaths.fcast)


# A simpler approach to seasonal dummy variables
deaths.lm <- tslm(ldeaths ~ season)
ldeaths.fcast <- forecast(deaths.lm, h = 36)
plot(ldeaths.fcast)

```
