# A unit test for mean forecasting
test_that("test meanf()", {
  meanfc <- mean(wineind)
  expect_true(all(meanf(wineind)$mean == meanfc))
  bcforecast <- meanf(wineind, lambda = -0.5)$mean
  expect_true(max(bcforecast) == min(bcforecast))
  expect_true(all(meanf(wineind, fan = TRUE)$mean == meanfc))
  expect_error(meanf(wineind, level = -10))
  expect_error(meanf(wineind, level = 110))
  # Constant series should not error
  series <- ts(rep(950, 20), frequency = 4)
  constantForecast <- expect_no_error(rwf(series))
  expect_true(is.constant(constantForecast$mean))
})

test_that("meanf() works with lambda = \"auto\"", {
  fc <- meanf(wineind, h = 5, lambda = "auto")
  fit <- mean_model(wineind, lambda = "auto")
  expect_equal(fc$lambda, fit$lambda)
  expect_equal(fc$mean, forecast(fit, h = 5)$mean)
})

test_that("test mean_model", {
  meanmod <- mean_model(wineind)
  f1 <- forecast(meanmod)
  f2 <- meanf(wineind)
  expect_equal(meanmod$mu, mean(wineind))
  expect_equal(meanmod$sigma, sd(wineind))
  expect_identical(f1$mean, f2$mean)
  expect_identical(f1$lower, f2$lower)
  expect_identical(f1$upper, f2$upper)
})

test_that("meanf() excludes missing values when estimating uncertainty", {
  y <- ts(c(NA, 1, NA, 2, 3, NA), start = c(2000, 1), frequency = 4)
  fc <- meanf(y, h = 2, level = 95)
  width <- qt(0.975, df = 2) * sqrt(1 + 1 / 3)
  expect_equal(c(fc$lower), rep(2 - width, 2))
  expect_equal(c(fc$upper), rep(2 + width, 2))
})

test_that("meanf() keeps series name with deprecated x argument", {
  expect_identical(meanf(x = USAccDeaths)$series, "USAccDeaths")
})
