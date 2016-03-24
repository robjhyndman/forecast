# A unit test for forecast.R
if(require(fpp) & require(testthat))
{
  context("Test forecast.R")
  test_that("tests for findfrequency()", {
    expect_true(frequency(oil) == findfrequency(as.numeric(oil)))
    expect_true(frequency(a10) == findfrequency(as.numeric(a10)))
    expect_true(frequency(woolyrnq) == findfrequency(as.numeric(woolyrnq)))
    expect_true(frequency(gas) == findfrequency(as.numeric(gas)))
    expect_true(frequency(wmurders) == findfrequency(ts(wmurders, f = 12)))
})

  test_that("tests forecast.ts()", {
    fc1 <- as.numeric(forecast(as.numeric(oil), find.frequency = TRUE)$mean)
    fc2 <- as.numeric(forecast(oil)$mean)
    expect_true(all(fc1 == fc2))
  })
  
  test_that("tests summary.forecast() and forecast.forecast()", {
    austaforecast <- forecast(austa)
    expect_output(summary(austaforecast), regexp = "Forecast method:")
    expect_true(all(predict(austaforecast)$mean == forecast(austaforecast)$mean))
  })
  
# test_that("tests plot.forecast()", {
#   # Fit several types of models for plotting
#   batsmod <- bats(cafe)
#   nnetmod <- nnetar(cafe)
#   tslmmod <- tslm(cafe ~ trend + season)
#   nnetfc<- forecast(nnetmod)
#   batsfc <- forecast(batsmod)
#   tslmfc <- forecast(tslmmod)
#   skip_on_travis()
#   # Plot the forecasts
#   expect_that(plot(nnetfc), not(throws_error()))
#   expect_that(plot(batsfc), not(throws_error()))
#   expect_that(plot(batsfc, shaded = FALSE), not(throws_error()))
#   expect_that(plot(tslmfc, plot.conf = FALSE), not(throws_error()))
#   expect_that(plot(forecast(tslmmod, h = 0)), not(throws_error()))
# })
}
