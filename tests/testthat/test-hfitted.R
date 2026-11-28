# A unit test for h-step fits
test_that("variance test on h-step fits", {
  mod1 <- ets(WWWusage, model = "AAN", damped = TRUE)
  h1 <- fitted(mod1, h = 1)
  h2 <- fitted(mod1, h = 2)
  j <- !is.na(h1) & !is.na(h2)
  expect_lt(var(diff(h1[j])), var(diff(h2[j])))
  # hfitted automatic function selection
  h2_1 <- hfitted(mod1, h = 2)
  expect_identical(h2, h2_1)

  mod2 <- Arima(WWWusage, order = c(1, 1, 1))
  h1 <- fitted(mod2, h = 1)
  h2 <- fitted(mod2, h = 2)
  j <- !is.na(h1) & !is.na(h2)
  expect_lt(var(diff(h1[j])), var(diff(h2[j])))

  mod3 <- arfima(WWWusage)
  h1 <- fitted(mod3, h = 1)
  h2 <- fitted(mod3, h = 2)
  j <- !is.na(h1) & !is.na(h2)
  expect_lt(var(diff(h1[j])), var(diff(h2[j])))

  mod4 <- tbats(WWWusage)
  h1 <- fitted(mod4, h = 1)
  h2 <- fitted(mod4, h = 2)
  j <- !is.na(h1) & !is.na(h2)
  expect_lt(var(diff(h1[j])), var(diff(h2[j])))

  mod5 <- bats(WWWusage)
  h1 <- fitted(mod5, h = 1)
  h2 <- fitted(mod5, h = 2)
  j <- !is.na(h1) & !is.na(h2)
  expect_lt(var(diff(h1[j])), var(diff(h2[j])))

  mod6 <- nnetar(WWWusage)
  h1 <- fitted(mod6, h = 1)
  h2 <- fitted(mod6, h = 2)
  j <- !is.na(h1) & !is.na(h2)
  expect_lt(var(diff(h1[j])), var(diff(h2[j])))
})

test_that("hfitted errors when no model function found", {
  obj <- structure(list(), class = "nonexistent_model_class")
  obj$fitted <- ts(1:10)
  expect_error(hfitted(obj, h = 2), "Can't find a function to refit the model")
})

test_that("multi-step ARIMA fits are back-transformed when lambda is used", {
  fit_lambda <- Arima(
    AirPassengers,
    order = c(0, 1, 1),
    seasonal = c(0, 1, 1),
    lambda = 0
  )
  fit_log <- Arima(
    log(AirPassengers),
    order = c(0, 1, 1),
    seasonal = c(0, 1, 1)
  )
  # Same model fitted on the log scale; h-step fits must agree after exp()
  expect_equal(fitted(fit_lambda, h = 2), exp(fitted(fit_log, h = 2)))
  expect_equal(fitted(fit_lambda, h = 3), exp(fitted(fit_log, h = 3)))
})

test_that("multi-step ARIMA fits are bias-adjusted when biasadj = TRUE", {
  fit <- Arima(
    AirPassengers,
    order = c(0, 1, 1),
    seasonal = c(0, 1, 1),
    lambda = 0,
    biasadj = TRUE
  )
  # The refit-based method forecasts with bias adjustment from each origin
  expect_equal(
    fitted(fit, h = 2),
    forecast:::hfitted.default(fit, h = 2, FUN = "Arima")
  )
})
