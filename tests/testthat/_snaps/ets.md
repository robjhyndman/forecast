# ets() fits simple models to very short series

    Code
      fit <- ets(ts(c(3, 5, 4, 6, 7)), damped = TRUE)
    Condition
      Warning in `ets()`:
      Not enough data to use damping

