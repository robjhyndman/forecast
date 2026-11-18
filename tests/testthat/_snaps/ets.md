# ets() ignores seasonality for frequency greater than 24

    Code
      fit <- ets(y)
    Condition
      Warning in `ets()`:
      Seasonal models are not supported for data with frequency greater than 24, fitting a non-seasonal model. Try `stlf()` for seasonal forecasts.

---

    Code
      ets(y, model = "ZZA")
    Condition
      Error in `ets()`:
      ! Seasonal models are not supported for data with frequency greater than 24. Try `stlf()` for seasonal forecasts.

# ets() fits simple models to very short series

    Code
      fit <- ets(ts(c(3, 5, 4, 6, 7)), damped = TRUE)
    Condition
      Warning in `ets()`:
      Not enough data to use damping.

# ets() checks the class of model

    Code
      ets(lh, model = Arima(lh))
    Condition
      Error in `ets()`:
      ! `model` must be a three-character string or the output of a previous call to `ets()`.
    Code
      ets(lh, model = ses(lh))
    Condition
      Error in `ets()`:
      ! `model` must be a three-character string or the output of a previous call to `ets()`.
    Code
      ets(lh, model = c("ANN", "AAN"))
    Condition
      Error in `ets()`:
      ! `model` must be a three-character string or the output of a previous call to `ets()`.
    Code
      ets(lh, model = NA_character_)
    Condition
      Error in `ets()`:
      ! `model` must be a three-character string or the output of a previous call to `ets()`.
    Code
      ets(lh, model = "ANNX")
    Condition
      Error in `ets()`:
      ! `model` must be a three-character string or the output of a previous call to `ets()`.
    Code
      ets(cbind(lh, lh), model = Arima(lh))
    Condition
      Error in `ets()`:
      ! `model` must be a three-character string or the output of a previous call to `ets()`.

