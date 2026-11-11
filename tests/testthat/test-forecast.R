# A unit test for forecast.R
test_that("tests for findfrequency()", {
  expect_true(frequency(airmiles) == findfrequency(as.numeric(airmiles)))
  expect_false(frequency(wineind) == findfrequency(as.numeric(wineind)))
  expect_true(frequency(woolyrnq) == findfrequency(as.numeric(woolyrnq)))
  expect_true(frequency(gas) == findfrequency(as.numeric(gas)))
})

test_that("tests forecast.ts()", {
  fc1 <- as.numeric(forecast(as.numeric(airmiles), find.frequency = TRUE)$mean)
  fc2 <- as.numeric(forecast(airmiles)$mean)
  expect_identical(fc1, fc2)
})

test_that("tests summary.forecast() and forecast.forecast()", {
  WWWusageforecast <- forecast(WWWusage)
  expect_output(print(summary(WWWusageforecast)), regexp = "Forecast method:")
  expect_true(all(
    predict(WWWusageforecast)$mean == forecast(WWWusageforecast)$mean
  ))
})

test_that("tests plot.forecast()", {
  nnetfc <- forecast(nnetar(woolyrnq))
  etsfc <- forecast(ets(woolyrnq))
  tslmfc <- forecast(tslm(woolyrnq ~ trend + season))
  expect_no_error(plot(nnetfc))
  expect_no_error(plot(etsfc))
  expect_no_error(plot(etsfc, shaded = FALSE))
  expect_no_error(plot(tslmfc, PI = FALSE))
})

test_that("as.data.frame() stacks the series of an mforecast object", {
  set.seed(2)
  y <- ts(cbind(a = rnorm(60) + 1:60, b = rnorm(60) - 1:60), frequency = 12)
  fc <- forecast(tslm(y ~ trend), h = 3)

  df <- as.data.frame(fc)
  expect_identical(dim(df), c(6L, 7L))
  expect_identical(df$Time, rep(c("Jan 6", "Feb 6", "Mar 6"), 2))
  expect_identical(df$Series, rep(c("a", "b"), each = 3))
  expect_equal(
    df[["Point Forecast"]],
    c(fc$forecast$a$mean, fc$forecast$b$mean),
    ignore_attr = TRUE
  )
})
