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

test_that("croston_model with optimized alpha", {
  fit <- croston_model(y, alpha = NULL)
  expect_s3_class(fit, "croston_model")
  expect_true(fit$alpha >= 0 && fit$alpha <= 1)
  mse_opt <- mean(fit$residuals^2, na.rm = TRUE)
  mse_default <- mean(croston_model(y, alpha = 0.1)$residuals^2, na.rm = TRUE)
  expect_lte(mse_opt, mse_default)
})

test_that("croston_model optimized alpha works with type variants", {
  fit_sba <- croston_model(y, alpha = NULL, type = "sba")
  fit_sbj <- croston_model(y, alpha = NULL, type = "sbj")
  expect_true(fit_sba$alpha >= 0 && fit_sba$alpha <= 1)
  expect_true(fit_sbj$alpha >= 0 && fit_sbj$alpha <= 1)
})

test_that("croston_model with mean initialization", {
  fit_mean <- croston_model(y, init = "mean")
  fit_naive <- croston_model(y, init = "naive")
  expect_equal(fit_mean$init, "mean")
  expect_equal(fit_naive$init, "naive")
  expect_false(identical(fit_mean$fitted, fit_naive$fitted))
})

test_that("croston_model with numeric init", {
  fit <- croston_model(y, init = c(3, 2))
  expect_equal(fit$init, c(3, 2))
  expect_equal(fit$fit_demand[1], 3)
  expect_equal(fit$fit_interval[1], 2)
})

test_that("croston_model with numeric init and optimized alpha", {
  fit <- croston_model(y, alpha = NULL, init = c(3, 2))
  expect_true(fit$alpha >= 0 && fit$alpha <= 1)
  expect_equal(fit$fit_demand[1], 3)
  expect_equal(fit$fit_interval[1], 2)
})

test_that("croston_model errors on invalid init", {
  expect_error(croston_model(y, init = c(3)), "length 2")
  expect_error(croston_model(y, init = c(-1, 2)), "non-negative")
  expect_error(croston_model(y, init = c(3, 0.5)), "at least 1")
})

test_that("croston_model with mae optimization", {
  fit_mse <- croston_model(y, alpha = NULL, opt.crit = "mse")
  fit_mae <- croston_model(y, alpha = NULL, opt.crit = "mae")
  expect_true(fit_mse$alpha >= 0 && fit_mse$alpha <= 1)
  expect_true(fit_mae$alpha >= 0 && fit_mae$alpha <= 1)
  mae_mae <- mean(abs(fit_mae$residuals), na.rm = TRUE)
  mae_mse <- mean(abs(fit_mse$residuals), na.rm = TRUE)
  expect_lte(mae_mae, mae_mse)
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

test_that("croston shortcut passes init and opt.crit", {
  fc <- croston(y, alpha = NULL, init = "mean", opt.crit = "mae")
  expect_s3_class(fc, "forecast")
})

test_that("croston shortcut with type variants", {
  fc_sba <- croston(y, h = 5, type = "sba")
  fc_sbj <- croston(y, h = 5, type = "sbj")
  fc_cr <- croston(y, h = 5, type = "croston")
  expect_true(fc_cr$mean[1] > fc_sba$mean[1])
  expect_true(fc_sba$mean[1] > fc_sbj$mean[1])
})
