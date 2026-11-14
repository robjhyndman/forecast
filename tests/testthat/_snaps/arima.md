# forecast.Arima() warns once about unknown arguments

    Code
      fc <- forecast(fit, h = 2, foo = 1, bar = 2)
    Condition
      Warning in `forecast.forecast_ARIMA()`:
      The non-existent foo, bar arguments will be ignored.

