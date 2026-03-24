mv_y <- ts(
  cbind(
    rnorm(120, 0, 3) + 1:120 + 20 * sin(2 * pi * (1:120) / 12),
    rnorm(120, 3, 7) + 1:120 + 16 * sin(2 * pi * (1:120 + 6) / 12)
  ),
  frequency = 12
)
v_y <- ts(
  rnorm(120, 0, 8) + (1:120) / 2 + 12 * sin(2 * pi * (1:120) / 12),
  frequency = 12
)
v_x <- ts(
  rnorm(120, 0, 1) + (1:120) * (-1) + 28 * sin(2 * pi * (1:120) / 12),
  frequency = 12
)

data <- datamat(mv_y, v_y, v_x, fourier(v_y, 3))

test_that("tests on model building with univariate time series", {
  fit1 <- tslm(v_y ~ trend + season, data = data)
  fit2 <- tslm(v_y ~ trend + season, data = data, lambda = 2, biasadj = FALSE)
  fit3 <- tslm(v_y ~ trend + season, data = data, lambda = 2, biasadj = TRUE)
  expect_false(identical(fit2$fitted.values, fit3$fitted.values))
  fit2 <- tslm(v_y ~ trend + season, data = data.frame(trend = rnorm(120)))
  expect_false(identical(fit1$model, fit2$model))
  fit2 <- tslm(v_y ~ trend + season)
  expect_named(fit1, names(fit2))
  expect_identical(fit1$model, fit2$model, ignore_attr = "terms")
  expect_identical(fit1$coefficients, fit2$coefficients)
  fit1 <- tslm(USAccDeaths ~ trend + season, data = USAccDeaths)
  fit2 <- tslm(USAccDeaths ~ trend + season)
  expect_named(fit1, names(fit2))
  expect_identical(fit1$model, fit2$model, ignore_attr = "terms")
  expect_identical(fit1$coefficients, fit2$coefficients)
  expect_warning(
    tslm(
      USAccDeaths ~ trend + season,
      data = USAccDeaths,
      subset = time(USAccDeaths) %% 1 < 0.1
    )
  )
  fit <- tslm(USAccDeaths ~ trend + season + trend * season, data = USAccDeaths)
  expect_true("trend:season" %in% attr(fit$terms, "term.labels"))
})

test_that("tslm parity with lm", {
  fit1 <- tslm(v_y ~ v_x + fourier(v_y, 3), data = data.frame(v_y = v_y))
  fit2 <- lm(v_y ~ v_x + fourier(v_y, 3), data = data.frame(v_y = v_y))
  expect_equal(fit1$coefficients, fit2$coefficients)
  expect_equal(fit1$model, fit2$model, ignore_attr = "terms")
})

test_that("tests on subsetting data", {
  a <- mv_y[, 1]
  expect_warning(
    tslm(mv_y ~ trend, subset = a < 20),
    "Subset has been assumed contiguous"
  )
  expect_error(
    tslm(mv_y ~ trend, subset = subset(mv_y, mv_y[, 1] < 20))
  )
  expect_warning(
    tslm(v_y ~ trend + season + trend * season, subset = v_y < 100),
    "Subset has been assumed contiguous"
  )
})

test_that("tests on model building with multivariate time series", {
  fit1 <- tslm(mv_y ~ trend + season)
  fit2 <- tslm(mv_y ~ trend + season, lambda = 0.5)
  expect_false(identical(fit1$coefficients, fit2$coefficients))
  fit3 <- tslm(mv_y ~ trend + season, lambda = 0.5, biasadj = TRUE)
  expect_false(identical(fit2$fitted.values, fit3$fitted.values))
  fit2 <- tslm(mv_y ~ trend + season, data = data)
  expect_named(fit1, names(fit2))
  expect_identical(fit1$model, fit2$model, ignore_attr = "terms")
  expect_identical(fit1$coefficients, fit2$coefficients)
  expect_warning(
    fit3 <- tslm(mv_y ~ trend + season, subset = mv_y[, 1] < 1),
    "Subset has been assumed contiguous"
  )
  expect_warning(
    fit4 <- tslm(mv_y ~ trend + season, data = data, subset = mv_y[, 1] < 1),
    "Subset has been assumed contiguous"
  )
  expect_named(fit3, names(fit4))
  expect_identical(fit3$model, fit4$model, ignore_attr = "terms")
  expect_identical(fit3$coefficients, fit4$coefficients)
})

test_that("tests with bad input", {
  expect_error(tslm(mpg ~ cyl, data = mtcars), "Not time series data")
})

test_that("forecast.lm", {
  fit1 <- tslm(v_y ~ trend + season, lambda = 2, biasadj = FALSE)
  fit2 <- tslm(v_y ~ trend + season, lambda = 2, biasadj = TRUE)
  fcast1 <- forecast(fit1, h = 60, biasadj = FALSE)
  fcast2 <- forecast(fit2, h = 60, biasadj = TRUE)
  expect_false(identical(fcast1$mean, fcast2$mean))

  fred <- tslm(ldeaths ~ trend + season, lambda = 0)
  fc <- forecast(fred)
  expect_s3_class(fc, "forecast")
})

test_that("tslm with function terms in formula", {
  expect_silent(tslm(v_y ~ trend + v_x + I(v_x^2) + fourier(v_x, 3)))
})

test_that("Missing values", {
  USMissingDeaths <- USAccDeaths
  USMissingDeaths[c(1, 44, 72)] <- NA
  timetrend <- 1:72
  fit <- tslm(USMissingDeaths ~ season + timetrend)
  expect_equal(sum(is.na(residuals(fit))), 3)
  fc <- forecast(fit, newdata = data.frame(timetrend = 73))
  expect_length(fc$mean, 1)
})

test_that("fitted.tslm returns fitted values", {
  fit <- tslm(USAccDeaths ~ trend + season)
  expect_identical(fitted(fit), fit$fitted.values)
  expect_s3_class(fitted(fit), "ts")
})

test_that("summary.tslm works", {
  fit <- tslm(USAccDeaths ~ trend + season)
  s <- summary(fit)
  expect_s3_class(s, "summary.lm")
  expect_false(is.null(s$adj.r.squared))
})

test_that("summary.tslm with lambda", {
  fit <- tslm(USAccDeaths ~ trend + season, lambda = 0)
  s <- summary(fit)
  expect_s3_class(s, "summary.lm")
})

test_that("CV returns named numeric vector", {
  fit <- tslm(USAccDeaths ~ trend + season)
  cv <- CV(fit)
  expect_named(cv, c("CV", "AIC", "AICc", "BIC", "AdjR2"))
  expect_length(cv, 5)
  expect_true(all(is.finite(cv)))
})

test_that("CV errors for non-lm objects", {
  expect_error(CV(1:10), "This function is for objects of class lm")
})

test_that("CV works with plain lm", {
  fit <- lm(mpg ~ cyl + disp, data = mtcars)
  cv <- CV(fit)
  expect_named(cv, c("CV", "AIC", "AICc", "BIC", "AdjR2"))
  expect_true(cv["AdjR2"] > 0 && cv["AdjR2"] < 1)
})

test_that("forecast.lm with multiple levels", {
  fit <- tslm(USAccDeaths ~ trend + season)
  fc <- forecast(fit, h = 12, level = c(80, 95))
  expect_equal(fc$level, c(80, 95))
  expect_equal(NCOL(fc$lower), 2)
  expect_equal(NCOL(fc$upper), 2)
  expect_s3_class(fc$mean, "ts")
})

test_that("forecast.lm with fan", {
  fit <- tslm(USAccDeaths ~ trend + season)
  fc <- forecast(fit, h = 12, fan = TRUE)
  expect_length(fc$level, 17)
})

test_that("forecast.lm errors with h < 1", {
  fit <- tslm(USAccDeaths ~ trend + season)
  expect_error(forecast(fit, h = 0), "The forecast horizon must be at least 1")
})

test_that("forecast.lm with newdata", {
  fit <- tslm(v_y ~ trend + season + v_x)
  nd <- data.frame(v_x = ts(rnorm(6), frequency = 12))
  fc <- forecast(fit, newdata = nd)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 6)
})

test_that("forecast.lm ts attributes are correct", {
  fit <- tslm(USAccDeaths ~ trend + season)
  fc <- forecast(fit, h = 24)
  expect_equal(frequency(fc$mean), 12)
  expect_equal(frequency(fc$lower), 12)
  expect_equal(frequency(fc$upper), 12)
  expect_length(fc$mean, 24)
})

test_that("tslm with non-seasonal data and trend only", {
  y <- ts(1:50 + rnorm(50), frequency = 1)
  fit <- tslm(y ~ trend)
  expect_s3_class(fit, "tslm")
  fc <- forecast(fit, h = 5)
  expect_length(fc$mean, 5)
})

test_that("tslm errors on non-seasonal data with season", {
  y <- ts(1:50 + rnorm(50), frequency = 1)
  expect_error(
    tslm(y ~ trend + season),
    "Non-seasonal data cannot be modelled using a seasonal factor"
  )
})

test_that("tslm with string formula", {
  fit <- tslm("USAccDeaths ~ trend + season")
  expect_s3_class(fit, "tslm")
})
