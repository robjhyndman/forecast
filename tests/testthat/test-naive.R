test_that("rw_model works", {
  fit <- rw_model(gold)
  expect_s3_class(fit, "rw_model")
  expect_false(fit$par$includedrift)
  expect_length(fitted(fit), length(gold))
  expect_length(residuals(fit), length(gold))
})

test_that("rw_model with drift", {
  fit <- rw_model(gold, drift = TRUE)
  expect_true(fit$par$includedrift)
  expect_equal(fit$method, "Lag walk with drift")
})

test_that("rw_model rejects multivariate ts", {
  expect_error(rw_model(ts(matrix(1:10, ncol = 2))), "Multivariate")
})

test_that("forecast.rw_model works", {
  fit <- rw_model(gold)
  fc <- forecast(fit, h = 10)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 10)
  expect_shape(fc$lower, ncol = 2)
  expect_shape(fc$upper, ncol = 2)
})

test_that("rwf works", {
  fc <- rwf(gold, h = 10)
  expect_s3_class(fc, "forecast")
  expect_equal(fc$method, "Random walk")
  fc_drift <- rwf(gold, h = 10, drift = TRUE)
  expect_equal(fc_drift$method, "Random walk with drift")
})

test_that("naive works", {
  fc <- naive(gold, h = 10)
  expect_s3_class(fc, "forecast")
  expect_equal(fc$method, "Naive method")
  expect_length(fc$mean, 10)
  # point forecasts equal last observation
  expect_true(all(fc$mean == tail(gold[!is.na(gold)], 1)))
})

test_that("snaive works", {
  fc <- snaive(wineind, h = 12)
  expect_s3_class(fc, "forecast")
  expect_equal(fc$method, "Seasonal naive method")
  expect_length(fc$mean, 12)
})

test_that("rwf with lambda", {
  fc <- rwf(gold, h = 10, lambda = 0)
  expect_s3_class(fc, "forecast")
  expect_false(is.null(fc$lambda))
})
