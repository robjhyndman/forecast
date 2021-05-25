# A unit test for forecast2.R
if (require(testthat)) {
  context("Test forecast2.R")
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
    constantForecast <- expect_error(rwf(series), NA)
    expect_true(is.constant(constantForecast$mean))
  })

  test_that("test rwf()", {
    rwfc <- rwf(airmiles)$mean
    expect_true(all(rwfc == naive(airmiles)$mean))
    expect_true(all(rwfc < rwf(airmiles, drift = TRUE)$mean))
    expect_true(all(rwf(airmiles, fan = TRUE)$mean == rwfc))
    expect_true(length(rwf(airmiles, lambda = 0.15)$mean) == 10)
    expect_false(identical(rwf(airmiles, lambda = 0.15, biasadj = FALSE)$mean, rwf(airmiles, lambda = 0.15, biasadj = TRUE)$mean))
    # Constant series should not error
    series <- ts(rep(950, 20), frequency = 4)
    constantForecast <- expect_error(rwf(series), NA)
    expect_true(is.constant(constantForecast$mean))
  })

  test_that("test forecast.HoltWinters()", {
    hwmod <- stats::HoltWinters(UKgas)
    expect_true(all(forecast(hwmod, fan = TRUE)$mean == forecast(hwmod)$mean))
    expect_error(forecast(hwmod, level = -10))
    expect_error(forecast(hwmod, level = 110))
    # Forecasts transformed manually with Box-Cox should match
    # forecasts when lambda is passed as an argument
    hwmodbc <- stats::HoltWinters(BoxCox(UKgas, lambda = 0.25))
    hwfc <- forecast(hwmodbc, lambda = 0.25, biasadj = FALSE)$mean
    hwfc2 <- forecast(hwmodbc, lambda = 0.25, biasadj = TRUE)$mean
    hwbcfc <- InvBoxCox(forecast(hwmodbc)$mean, lambda = 0.25)
    expect_true(all(hwfc == hwbcfc))
    expect_false(identical(hwfc, hwfc2))
  })

  test_that("test for forecast.StructTS()", {
    structtsmod <- stats::StructTS(wineind)
    fc1 <- forecast(structtsmod)$mean
    expect_true(all(fc1 == forecast(structtsmod, fan = TRUE)$mean))
    expect_error(forecast(structtsmod, level = -10))
    expect_error(forecast(structtsmod, level = 110))
    # Forecasts transformed manually with Box-Cox should match
    # forecasts when lambda is passed as an argument
    bcseries <- BoxCox(woolyrnq, lambda = 0.19)
    fc2 <- InvBoxCox(forecast(stats::StructTS(bcseries))$mean, lambda = 0.19)
    fc3 <- forecast(stats::StructTS(bcseries), lambda = 0.19, biasadj = FALSE)$mean
    fc4 <- forecast(stats::StructTS(bcseries), lambda = 0.19, biasadj = TRUE)$mean
    expect_true(all(fc2 == fc3))
    expect_false(identical(fc3, fc4))
  })

  test_that("test croston()", {
    set.seed(1234)
    expect_error(croston(rnorm(100)))
    expect_true(all(croston(rep(0, 100))$mean == 0))
  })

  test_that("test hw()", {
    expect_output(print(summary(holt(wineind))), regexp = "Forecast method: Holt's method")
    expect_output(print(summary(holt(wineind, damped = TRUE))), regexp = "Forecast method: Damped Holt's method")
  })

  test_that("test holt()", {
    expect_output(print(summary(hw(wineind))), regexp = "Forecast method: Holt-Winters' additive method")
  })

  test_that("test naive() and snaive()", {
    # WWWusage has frequency = 1, so naive and snaive should match
    expect_true(all(snaive(WWWusage, h = 10)$mean == naive(WWWusage)$mean))
    expect_true(all(snaive(WWWusage, h = 10)$upper == naive(WWWusage)$upper))
    expect_true(all(snaive(WWWusage, h = 10)$lower == naive(WWWusage)$lower))
    # Constant series should not error
    series <- ts(rep(950, 20), frequency = 4)
    constantForecast <- expect_error(snaive(series), NA)
    expect_true(is.constant(constantForecast$mean))
  })
}
