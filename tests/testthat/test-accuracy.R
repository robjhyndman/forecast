# A unit test for accuracy() function
if (require(testthat)) {
  context("Tests on input")
  test_that("tests for a non-forecast object", {
    expect_that(accuracy(USAccDeaths), throws_error())
  })

  context("Tests on output")
  test_that("tests for dimension", {
    train <- window(USAccDeaths, start = c(1973, 1), end = c(1976, 12))
    test <- window(USAccDeaths, start = c(1977, 1))
    fcasts <- forecast(train, h = 6)
    expect_that(dim(accuracy(fcasts)), equals(c(1, 7)))
    expect_that(dim(accuracy(fcasts, test)), equals(c(2, 8)))
    expect_false(
      all(dim(accuracy(fcasts, test, test = 1:2)) == dim(accuracy(fcasts, test)))
    )
    expect_that(accuracy(fcasts, test = 1:length(train)), equals(accuracy(fcasts)))
  })
  test_that("tests for accuracy", {
    # Test arima
    fitarima <- Arima(USAccDeaths, order = c(0, 1, 1), seasonal = c(0, 1, 1))
    accuracyarima <- accuracy(fitarima)[1, "RMSE"]
    accuracyarimasim <- accuracy(Arima(simulate(fitarima, seed = 123), order = c(0, 1, 0), seasonal = c(0, 0, 1)))[1, "RMSE"]
    expect_lt(accuracyarima, accuracyarimasim)
    # Test ets
    fitets <- ets(AirPassengers, model = "MAM", damped = TRUE)
    accuracyets <- accuracy(fitets)[1, "RMSE"]
    accuracyetssim <- accuracy(ets(simulate(fitets, seed = 123), model = "MAM", damped = TRUE))[1, "RMSE"]
    expect_lt(accuracyets, accuracyetssim)
    # Test lm
    month <- factor(rep(1:12, 14))
    fitlm <- lm(wineind[1:168] ~ month)
    accuracylm <- accuracy(fitlm)[1, "RMSE"]
    accuracylmsim <- accuracy(lm(simulate(fitlm, seed = 123)[, 1] ~ month))[1, "RMSE"]
    expect_gt(accuracylm, accuracylmsim)
  })
}
