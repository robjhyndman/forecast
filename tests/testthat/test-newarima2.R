# A unit test functions in newarima2.R
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
  expect_output(
    print(auto.arima(testseries2, xreg = xregmat)),
    regexp = "Series: testseries2"
  )

  expect_output(
    print(summary(auto.arima(
      testseries2,
      xreg = xregmat,
      approximation = TRUE,
      stepwise = FALSE
    ))),
    regexp = "Series: testseries2"
  )
  expect_output(
    print(auto.arima(
      ts(testseries2, frequency = 4),
      approximation = TRUE,
      trace = TRUE
    )),
    regexp = "ARIMA"
  )

  fit1 <- auto.arima(testseries1, stepwise = FALSE, lambda = 2, biasadj = FALSE)
  fit2 <- auto.arima(testseries1, stepwise = FALSE, lambda = 2, biasadj = TRUE)
  expect_false(identical(fit1$fitted, fit2$fitted))
})

test_that("auto.arima() truncates xreg alongside x with approximation", {
  set.seed(1)
  n <- 200
  xreg <- matrix(rnorm(n), ncol = 1)
  y <- ts(cumsum(rnorm(n)) + 0.5 * xreg[, 1], frequency = 12)
  fit <- auto.arima(y, xreg = xreg, approximation = TRUE, truncate = 100)
  expect_s3_class(fit, "Arima")
})

test_that("auto.arima() refits the selected model to all data when truncating", {
  set.seed(2)
  n <- 300
  xreg <- matrix(rnorm(n), dimnames = list(NULL, "z"))
  y <- ts(cumsum(rnorm(n, mean = 1)) + 2 * xreg[, 1])
  for (stepwise in c(TRUE, FALSE)) {
    fit <- auto.arima(
      y,
      d = 1,
      xreg = xreg,
      approximation = TRUE,
      truncate = 50,
      stepwise = stepwise
    )
    expect_equal(getResponse(fit), y)
    expect_length(residuals(fit), n)
    ref <- Arima(
      y,
      order = arimaorder(fit),
      xreg = xreg,
      include.drift = "drift" %in% names(coef(fit))
    )
    expect_equal(coef(fit), coef(ref), tolerance = 1e-6)
    newxreg <- matrix(rnorm(5), dimnames = list(NULL, "z"))
    expect_equal(
      forecast(fit, xreg = newxreg)$mean,
      forecast(ref, xreg = newxreg)$mean,
      tolerance = 1e-6
    )
  }
})

test_that("auto.arima() errors when all data are missing", {
  expect_snapshot(auto.arima(ts(rep(NA_real_, 20))), error = TRUE)
})

test_that("test parallel = FALSE and stepwise = FALSE for auto.arima()", {
  skip_on_ci()
  expect_equal(
    auto.arima(WWWusage, parallel = FALSE, stepwise = FALSE)$arma,
    c(3L, 0L, 0L, 0L, 1L, 1L, 0L)
  )
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

test_that("auto.arima() keeps series name with deprecated x argument", {
  expect_identical(auto.arima(x = WWWusage)$series, "WWWusage")
})
