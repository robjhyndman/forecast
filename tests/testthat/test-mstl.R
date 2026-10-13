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
