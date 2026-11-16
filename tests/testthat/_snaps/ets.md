# ets() fits simple models to very short series

    Code
      fit <- ets(ts(c(3, 5, 4, 6, 7)), damped = TRUE)
    Condition
      Warning in `ets()`:
      Not enough data to use damping

# ets() checks the class of model

    Code
      ets(lh, model = Arima(lh))
    Condition
      Error in `ets()`:
      ! model should be a three-character string or the output of a previous call to ets()
    Code
      ets(lh, model = ses(lh))
    Condition
      Error in `ets()`:
      ! model should be a three-character string or the output of a previous call to ets()
    Code
      ets(lh, model = c("ANN", "AAN"))
    Condition
      Error in `ets()`:
      ! model should be a three-character string or the output of a previous call to ets()
    Code
      ets(lh, model = NA_character_)
    Condition
      Error in `ets()`:
      ! model should be a three-character string or the output of a previous call to ets()
    Code
      ets(lh, model = "ANNX")
    Condition
      Error in `ets()`:
      ! model should be a three-character string or the output of a previous call to ets()
    Code
      ets(cbind(lh, lh), model = Arima(lh))
    Condition
      Error in `ets()`:
      ! model should be a three-character string or the output of a previous call to ets()

