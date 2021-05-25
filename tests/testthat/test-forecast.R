# A unit test for forecast.R
if (require(testthat)) {
  context("Test forecast.R")
  test_that("tests for findfrequency()", {
    expect_true(frequency(airmiles) == findfrequency(as.numeric(airmiles)))
    expect_false(frequency(wineind) == findfrequency(as.numeric(wineind)))
    expect_true(frequency(woolyrnq) == findfrequency(as.numeric(woolyrnq)))
    expect_true(frequency(gas) == findfrequency(as.numeric(gas)))
  })

  test_that("tests forecast.ts()", {
    fc1 <- as.numeric(forecast(as.numeric(airmiles), find.frequency = TRUE)$mean)
    fc2 <- as.numeric(forecast(airmiles)$mean)
    expect_true(all(fc1 == fc2))
  })

  test_that("tests summary.forecast() and forecast.forecast()", {
    WWWusageforecast <- forecast(WWWusage)
    expect_output(print(summary(WWWusageforecast)), regexp = "Forecast method:")
    expect_true(all(predict(WWWusageforecast)$mean == forecast(WWWusageforecast)$mean))
  })

  # test_that("tests plot.forecast()", {
  #   # Fit several types of models for plotting
  #   batsmod <- bats(woolyrnq)
  #   nnetmod <- nnetar(woolyrnq)
  #   tslmmod <- tslm(woolyrnq ~ trend + season)
  #   nnetfc<- forecast(nnetmod)
  #   batsfc <- forecast(batsmod)
  #   tslmfc <- forecast(tslmmod)
  #   skip_on_travis()
  #   # Plot the forecasts
  #   expect_that(plot(nnetfc), not(throws_error()))
  #   expect_that(plot(batsfc), not(throws_error()))
  #   expect_that(plot(batsfc, shaded = FALSE), not(throws_error()))
  #   expect_that(plot(tslmfc, PI = FALSE), not(throws_error()))
  #   expect_that(plot(forecast(tslmmod, h = 0)), not(throws_error()))
  # })
}
