# A unit test for accuracy() function
test_that("tests for a non-forecast object (input)", {
  expect_error(accuracy(USAccDeaths))
})

test_that("tests for dimension (output)", {
  train <- window(USAccDeaths, start = c(1973, 1), end = c(1976, 12))
  test <- window(USAccDeaths, start = c(1977, 1))
  fcasts <- forecast(train, h = 6)
  expect_shape(accuracy(fcasts), dim = c(1L, 7L))
  expect_shape(accuracy(fcasts, test), dim = c(2L, 8L))
  expect_false(
    all(dim(accuracy(fcasts, test, test = 1:2)) == dim(accuracy(fcasts, test)))
  )
  expect_identical(accuracy(fcasts, test = seq_along(train)), accuracy(fcasts))
})

test_that("tests for accuracy (output)", {
  # Test arima
  fitarima <- Arima(USAccDeaths, order = c(0, 1, 1), seasonal = c(0, 1, 1))
  accuracyarima <- accuracy(fitarima)[1, "RMSE"]
  accuracyarimasim <- accuracy(Arima(
    simulate(fitarima, seed = 123),
    order = c(0, 1, 0),
    seasonal = c(0, 0, 1)
  ))[1, "RMSE"]
  expect_lt(accuracyarima, accuracyarimasim)
  # Test ets
  fitets <- ets(AirPassengers, model = "MAM", damped = TRUE)
  accuracyets <- accuracy(fitets)[1, "RMSE"]
  accuracyetssim <- accuracy(ets(
    simulate(fitets, seed = 123),
    model = "MAM",
    damped = TRUE
  ))[1, "RMSE"]
  expect_lt(accuracyets, accuracyetssim)
  # Test lm
  month <- factor(rep(1:12, 14))
  fitlm <- lm(wineind[1:168] ~ month)
  accuracylm <- accuracy(fitlm)[1, "RMSE"]
  accuracylmsim <- accuracy(lm(simulate(fitlm, seed = 123)[, 1] ~ month))[
    1,
    "RMSE"
  ]
  expect_gt(accuracylm, accuracylmsim)
})

test_that("accuracy fc_model", {
  mods <- c(
    arfima,
    Arima,
    ets,
    bats,
    tbats,
    nnetar,
    stlm,
    baggedModel,
    rw_model,
    mean_model,
    croston_model,
    theta_model,
    spline_model
  )
  train <- window(USAccDeaths, start = c(1973, 1), end = c(1976, 12))
  test <- window(USAccDeaths, start = c(1977, 1))
  for (i in seq_along(mods)) {
    fit <- mods[[i]](train)
    fc <- forecast(fit)
    a <- accuracy(fit)
    b <- accuracy(fc)
    c <- accuracy(fc, test)
    expect_shape(a, dim = c(1, 7))
    expect_shape(c, dim = c(2, 8))
    expect_identical(a, b)
    expect_identical(b, c[1, 1:7, drop=FALSE])
    expect_lt(a[, "MASE"], 1.8)
    expect_lt(b[, "MASE"], 1.8)
    expect_identical(
      colnames(a),
      c(
        "ME",
        "RMSE",
        "MAE",
        "MPE",
        "MAPE",
        "MASE",
        "ACF1"
      )
    )
  }
})
