# nsdiffs warns and returns 0 for frequency less than 1

    Code
      d <- nsdiffs(ts(1:20, frequency = 0.5))
    Condition
      Warning in `nsdiffs()`:
      Seasonality is not supported for data with frequency less than 1, ignoring it.

