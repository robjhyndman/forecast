# A unit test functions in newarima2.R
if (require(testthat)) {
  test_that("test auto.arima() and associated methods", {
    expect_warning(auto.arima(rep(1, 100), stepwise = TRUE, parallel = TRUE))
    set.seed(345)
    testseries1 <- ts(rnorm(100) + 1:100, frequency = 0.1)
    xregmat <- matrix(runif(300), ncol = 3)
    expect_true(frequency(forecast(auto.arima(testseries1))) == 1)
    fit1 <- auto.arima(testseries1, xreg = xregmat, allowdrift = FALSE)
    expect_true(all(xregmat == fit1$xreg))

    testseries2 <- ts(rep(100, 120), frequency = 12)
    xregmat <- matrix(runif(240), ncol = 2)
    expect_output(print(auto.arima(testseries2, xreg = xregmat)), regexp = "Series: testseries2")

    expect_output(print(summary(auto.arima(testseries2, xreg = xregmat, approximation = TRUE, stepwise = FALSE))), regexp = "Series: testseries2")
    expect_output(print(auto.arima(ts(testseries2, frequency = 4), approximation = TRUE, trace = TRUE)), regexp = "ARIMA")

    fit1 <- auto.arima(testseries1, stepwise = FALSE, lambda = 2, biasadj = FALSE)
    fit2 <- auto.arima(testseries1, stepwise = FALSE, lambda = 2, biasadj = TRUE)
    expect_false(identical(fit1$fitted, fit2$fitted))
  })

  test_that("test parallel = TRUE and stepwise = FALSE for auto.arima()", {
    skip_if(identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
    expect_equal(auto.arima(WWWusage, parallel = TRUE, stepwise = FALSE)$arma, c(3L, 0L, 0L, 0L, 1L, 1L, 0L))
  })


  test_that("tests for ndiffs()", {
    expect_true(ndiffs(AirPassengers, test = "kpss") == 1)
    expect_true(ndiffs(AirPassengers, test = "adf") == 1)
    expect_true(ndiffs(AirPassengers, test = "pp") == 1)
  })

  test_that("tests for nsdiffs()", {
    expect_true(nsdiffs(AirPassengers, test = "seas") == 1)
    expect_true(nsdiffs(AirPassengers, test = "ocsb") == 1)
    expect_error(nsdiffs(airmiles))
    expect_true(nsdiffs(rep(1, 100)) == 0)
    expect_warning(nsdiffs(ts(rnorm(10), frequency = 0.1)))
    skip_if_not_installed("uroot")
    expect_true(nsdiffs(AirPassengers, test = "hegy") == 1)
    expect_true(nsdiffs(AirPassengers, test = "ch") == 0)
  })
}
