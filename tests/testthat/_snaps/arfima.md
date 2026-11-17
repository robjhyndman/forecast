# arfima() checks the class of model

    Code
      arfima(lh, model = Arima(lh))
    Condition
      Error in `arfima()`:
      ! model should be the output of a previous call to arfima()

# forecast.fracdiff() checks h

    Code
      forecast(fit, h = 0)
    Condition
      Error in `forecast.fracdiff()`:
      ! h must be a positive integer
    Code
      forecast(fit, h = 1.5)
    Condition
      Error in `forecast.fracdiff()`:
      ! h must be a positive integer
    Code
      forecast(fit, h = c(1, 2))
    Condition
      Error in `forecast.fracdiff()`:
      ! h must be a positive integer

