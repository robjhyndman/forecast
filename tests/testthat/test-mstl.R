test_that("forecast.mstl works without lambda", {
  fit <- mstl(AirPassengers)
  fc <- forecast(fit, h = 12)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 12)
  expect_null(fc$lambda)
})

test_that("forecast.mstl works with lambda", {
  fit <- mstl(AirPassengers, lambda = 0)
  fc <- forecast(fit, h = 12)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, 12)
  expect_equal(fc$lambda, 0, ignore_attr = TRUE)
})

test_that("stlf() and stlm() keep series name with deprecated x argument", {
  expect_identical(stlf(x = USAccDeaths)$series, "USAccDeaths")
  expect_identical(stlm(x = USAccDeaths)$series, "USAccDeaths")
})

test_that("forecast.stl and forecast.stlm order levels before forecasting", {
  rev_rwf <- function(x, h, level, ...) {
    expect_identical(level, c(80, 95))
    rwf(x, h = h, level = level)
  }
  fc <- stlf(USAccDeaths, h = 3, forecastfunction = rev_rwf, level = c(95, 80))
  expect_identical(fc$level, c(80, 95))
  fc <- forecast(stlm(USAccDeaths), h = 3, level = c(0.95, 0.8))
  expect_identical(fc$level, c(80, 95))
})
