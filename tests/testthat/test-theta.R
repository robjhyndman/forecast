test_that("theta_model fits non-seasonal data", {
  fit <- theta_model(Nile)
  expect_s3_class(fit, "theta_model")
  expect_s3_class(fit, "fc_model")
  expect_length(fit$fitted, length(Nile))
  expect_length(fit$residuals, length(Nile))
  expect_null(fit$seas_component)
  expect_null(fit$lambda)
  expect_true(is.numeric(fit$alpha))
  expect_true(is.numeric(fit$drift))
  expect_true(is.numeric(fit$sigma2))
  expect_output(print(fit), "Theta model")
})

test_that("theta_model fits seasonal data", {
  fit <- theta_model(USAccDeaths)
  expect_s3_class(fit, "theta_model")
  expect_false(is.null(fit$seas_component))
  expect_length(fit$seas_component, frequency(USAccDeaths))
  expect_output(print(fit), "Deseasonalized")
})

test_that("theta_model with lambda", {
  fit <- theta_model(Nile, lambda = 0)
  expect_equal(fit$lambda, 0, ignore_attr = TRUE)
  fit2 <- theta_model(Nile, lambda = "auto")
  expect_true(is.numeric(fit2$lambda))
})

test_that("theta_model with biasadj", {
  fit1 <- theta_model(Nile, lambda = 0, biasadj = FALSE)
  fit2 <- theta_model(Nile, lambda = 0, biasadj = TRUE)
  expect_false(identical(fit1$fitted, fit2$fitted))
})

test_that("forecast.theta_model non-seasonal", {
  fit <- theta_model(Nile)
  fc <- forecast(fit, h = 10)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 10)
  expect_equal(NCOL(fc$lower), 2)
  expect_equal(NCOL(fc$upper), 2)
  expect_equal(fc$level, c(80, 95))
  expect_identical(fc$x, Nile)
})

test_that("forecast.theta_model seasonal", {
  fit <- theta_model(USAccDeaths)
  fc <- forecast(fit, h = 24)
  expect_length(fc$mean, 24)
  expect_equal(frequency(fc$mean), 12)
})

test_that("forecast.theta_model with fan", {
  fit <- theta_model(Nile)
  fc <- forecast(fit, fan = TRUE)
  expect_length(fc$level, 17)
  expect_equal(NCOL(fc$lower), 17)
})

test_that("forecast.theta_model with lambda", {
  fit <- theta_model(Nile, lambda = 0)
  fc <- forecast(fit, h = 5)
  expect_true(all(fc$mean > 0))
  expect_true(all(fc$lower > 0))
})

test_that("forecast.theta_model default h", {
  fit_ns <- theta_model(Nile)
  fc_ns <- forecast(fit_ns)
  expect_length(fc_ns$mean, 10)
  fit_s <- theta_model(USAccDeaths)
  fc_s <- forecast(fit_s)
  expect_length(fc_s$mean, 24)
})

test_that("thetaf shortcut", {
  fc <- thetaf(Nile, h = 5)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 5)
})

test_that("thetaf with lambda", {
  fc <- thetaf(Nile, h = 5, lambda = 0)
  expect_true(all(fc$mean > 0))
})

test_that("thetaf seasonal", {
  fc <- thetaf(USAccDeaths, h = 12)
  expect_length(fc$mean, 12)
  expect_equal(frequency(fc$mean), 12)
})
