# Return the order of an ARIMA or ARFIMA model

Returns the order of a univariate ARIMA or ARFIMA model.

## Usage

``` r
arimaorder(object)
```

## Arguments

- object:

  An object of class `Arima`, `ar` or `fracdiff`. Usually the result of
  a call to [`stats::arima()`](https://rdrr.io/r/stats/arima.html),
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md),
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
  [`stats::ar()`](https://rdrr.io/r/stats/ar.html),
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  or
  [`fracdiff::fracdiff()`](https://rdrr.io/pkg/fracdiff/man/fracdiff.html).

## Value

A numerical vector giving the values \\p\\, \\d\\ and \\q\\ of the ARIMA
or ARFIMA model. For a seasonal ARIMA model, the returned vector
contains the values \\p\\, \\d\\, \\q\\, \\P\\, \\D\\, \\Q\\ and \\m\\,
where \\m\\ is the period of seasonality.

## See also

[`stats::ar()`](https://rdrr.io/r/stats/ar.html),
[auto.arima](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md),
[`stats::arima()`](https://rdrr.io/r/stats/arima.html),
[`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md).

## Author

Rob J Hyndman

## Examples

``` r
WWWusage |> auto.arima() |> arimaorder()
#> p d q 
#> 1 1 1 
```
