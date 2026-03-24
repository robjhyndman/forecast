y <- c(0, 0, 3, 0, 0, 0, 2, 0, 5, 0, 0, 1, 0, 0, 0, 0, 4, 0, 0, 3)

test_that("croston_model basic fitting", {
  fit <- croston_model(y)
  expect_s3_class(fit, "croston_model")
  expect_s3_class(fit, "fc_model")
  expect_equal(fit$alpha, 0.1)
  expect_equal(fit$type, "croston")
  expect_length(fit$fitted, length(y))
  expect_length(fit$residuals, length(y))
  expect_true(is.na(fit$fitted[1]))
  expect_equal(as.numeric(fit$residuals), as.numeric(y - fit$fitted))
})

test_that("croston_model with custom alpha", {
  fit <- croston_model(y, alpha = 0.5)
  expect_equal(fit$alpha, 0.5)
})

test_that("croston_model type variants", {
  fit_cr <- croston_model(y, type = "croston")
  fit_sba <- croston_model(y, type = "sba")
  fit_sbj <- croston_model(y, type = "sbj")
  expect_equal(fit_cr$type, "croston")
  expect_equal(fit_sba$type, "sba")
  expect_equal(fit_sbj$type, "sbj")
  last_cr <- fit_cr$fitted[length(y)]
  last_sba <- fit_sba$fitted[length(y)]
  last_sbj <- fit_sbj$fitted[length(y)]
  expect_gt(last_cr, last_sba)
  expect_gt(last_sba, last_sbj)
})

test_that("croston_model errors on invalid input", {
  expect_error(croston_model(y, alpha = -0.1), "alpha must be between 0 and 1")
  expect_error(croston_model(y, alpha = 1.5), "alpha must be between 0 and 1")
  expect_error(croston_model(c(-1, 2, 0)), "non-negative data")
  expect_error(croston_model(c(0, 0, 1, 0)), "At least two non-zero values")
  expect_error(croston_model(c(0, 0, 0)), "At least two non-zero values")
})

test_that("print.croston_model", {
  fit <- croston_model(y)
  expect_output(print(fit), "alpha")
  expect_output(print(fit), "method")
})

test_that("forecast.croston_model", {
  fit <- croston_model(y)
  fc <- forecast(fit, h = 12)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 12)
  expect_true(all(fc$mean == fc$mean[1]))
  expect_equal(fc$mean[1], fit$fitted[length(y)])
  expect_identical(fc$x, fit$y)
})

test_that("forecast.croston_model preserves ts attributes", {
  yts <- ts(y, frequency = 4)
  fit <- croston_model(yts)
  fc <- forecast(fit, h = 8)
  expect_equal(frequency(fc$mean), 4)
})

test_that("croston shortcut", {
  fc <- croston(y, h = 5)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 5)
})

test_that("croston shortcut with type variants", {
  fc_sba <- croston(y, h = 5, type = "sba")
  fc_sbj <- croston(y, h = 5, type = "sbj")
  fc_cr <- croston(y, h = 5, type = "croston")
  expect_true(fc_cr$mean[1] > fc_sba$mean[1])
  expect_true(fc_sba$mean[1] > fc_sbj$mean[1])
})
