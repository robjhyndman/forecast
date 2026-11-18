# ARIMA errors fail clearly when the regressors can't be found

    Code
      residuals(fit, type = "regression")
    Condition
      Error:
      ! Can't find regressors matching the data this model was fitted to. Use `Arima()` or `auto.arima()` rather than `arima()`.

---

    Code
      residuals(fit, type = "regression")
    Condition
      Error:
      ! Can't find regressors matching the data this model was fitted to. Use `Arima()` or `auto.arima()` rather than `arima()`.

---

    Code
      residuals(fit, type = "regression")
    Condition
      Error:
      ! Can't find regressors matching the data this model was fitted to. Use `Arima()` or `auto.arima()` rather than `arima()`.

# forecast.Arima() warns once about unknown arguments

    Code
      fc <- forecast(fit, h = 2, foo = 1, bar = 2)
    Condition
      Warning in `forecast.forecast_ARIMA()`:
      The non-existent foo, bar arguments will be ignored.

# Arima() checks the class of model

    Code
      Arima(lh, model = ets(lh))
    Condition
      Error in `Arima()`:
      ! `model` must be the output of a previous call to `Arima()`, `auto.arima()` or `arima()`.

