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

test_that("forecast.forecast() accepts the levels used to compute the forecasts", {
  fc <- naive(gold, h = 5, level = c(95, 80))
  expect_identical(forecast(fc, h = 2, level = c(95, 80))$level, c(80, 95))
  expect_identical(forecast(fc, h = 2, level = c(80L, 95L))$level, c(80, 95))
  expect_snapshot(forecast(fc, level = 90), error = TRUE)
})

test_that("forecast.forecast() accepts forecasts with unsorted levels", {
  fc <- naive(gold, h = 5, level = c(95, 80))
  fc$level <- c(95, 80)
  expect_identical(forecast(fc, h = 2, level = c(95, 80))$level, c(95, 80))
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

test_that("forecast() methods take biasadj from the lambda in use", {
  y <- AirPassengers
  fits <- list(
    Arima = Arima(y, order = c(1, 1, 0), lambda = 0, biasadj = TRUE),
    ets = ets(y, lambda = 0, biasadj = TRUE),
    arfima = arfima(WWWusage, lambda = 0, biasadj = TRUE),
    stlm = stlm(y, lambda = 0, biasadj = TRUE),
    rw_model = rw_model(y, lambda = 0, biasadj = TRUE),
    mean_model = mean_model(y, lambda = 0, biasadj = TRUE),
    theta_model = theta_model(y, lambda = 0, biasadj = TRUE),
    spline_model = spline_model(y, lambda = 0, biasadj = TRUE),
    tslm = tslm(y ~ trend, lambda = 0, biasadj = TRUE)
  )
  for (name in names(fits)) {
    fit <- fits[[name]]
    adjusted <- forecast(fit, h = 2, biasadj = TRUE)$mean
    unadjusted <- forecast(fit, h = 2, biasadj = FALSE)$mean
    expect_equal(forecast(fit, h = 2)$mean, adjusted, info = name)
    expect_no_warning(fc <- forecast(fit, h = 2, lambda = 0))
    expect_equal(fc$mean, unadjusted, info = name)
    fc <- forecast(fit, h = 2, lambda = structure(0, biasadj = TRUE))
    expect_equal(fc$mean, adjusted, info = name)
  }
})

test_that("forecast() methods default to no bias adjustment", {
  y <- AirPassengers
  fits <- list(
    Arima = Arima(y, order = c(1, 1, 0)),
    ets = ets(y, model = "ANN"),
    arfima = arfima(WWWusage),
    stlm = stlm(y),
    rw_model = rw_model(y),
    mean_model = mean_model(y),
    theta_model = theta_model(y),
    spline_model = spline_model(y),
    tslm = tslm(y ~ trend)
  )
  for (name in names(fits)) {
    fit <- fits[[name]]
    expect_no_warning(fc <- forecast(fit, h = 2, lambda = 0))
    expect_equal(
      fc$mean,
      forecast(fit, h = 2, lambda = 0, biasadj = FALSE)$mean,
      info = name
    )
  }
})

test_that("forecast() uses the model's biasadj for bats, tbats, mstl and mlm", {
  y <- AirPassengers
  fits <- list(
    bats = bats(y, use.box.cox = TRUE, biasadj = TRUE, use.parallel = FALSE),
    tbats = tbats(y, use.box.cox = TRUE, biasadj = TRUE, use.parallel = FALSE),
    mstl = mstl(y, lambda = 0, biasadj = TRUE),
    mlm = tslm(cbind(mdeaths, fdeaths) ~ trend, lambda = 0, biasadj = TRUE)
  )
  for (name in names(fits)) {
    fit <- fits[[name]]
    expect_no_warning(fc <- forecast(fit, h = 2))
    expect_equal(
      fc$mean,
      forecast(fit, h = 2, biasadj = TRUE)$mean,
      info = name
    )
  }
})
