# A unit test for forecast.R
if(require(fpp) & require(testthat))
{
  test_that("tests for findfrequency()t", {
  expect_true(frequency(oil) == findfrequency(as.numeric(oil)))
  expect_true(frequency(a10) == findfrequency(as.numeric(a10)))
  expect_true(frequency(woolyrnq) == findfrequency(as.numeric(woolyrnq)))
  expect_true(frequency(gas) == findfrequency(as.numeric(gas)))
  expect_true(findfrequency(as.numeric(wineind)) == findfrequency(ts(wineind, f = 4)))
  expect_true(frequency(wmurders) == findfrequency(ts(wmurders, f = 12)))
})

test_that("tests summary.forecast() and forecast.forecast()", {
  austaforecast <- forecast(austa)
  expect_output(summary(austaforecast), regexp = "Forecast method:")
  expect_true(all(predict(austaforecast)$mean == forecast(austaforecast)$mean))
})
}
