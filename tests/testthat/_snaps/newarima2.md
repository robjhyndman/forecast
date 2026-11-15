# auto.arima() errors when all data are missing

    Code
      auto.arima(ts(rep(NA_real_, 20)))
    Condition
      Error in `auto.arima()`:
      ! All data are missing

