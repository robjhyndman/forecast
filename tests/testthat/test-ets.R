# A unit test for ets function
test_that("tests for some arguments in ets", {
  fit <- ets(wineind, model = "ZZM")
  comp <- paste0(fit$components[1:3], collapse = "")
  expect_identical(comp, "MAM")
})

test_that("tests for some arguments in ets", {
  fit <- ets(wineind, model = "MAM", alpha = 0.1611)
  expect_identical(as.numeric(fit$par["alpha"]), 0.1611)
})

test_that("refit ets model to new data", {
  fit <- ets(wineind, model = "MAM", alpha = 0.1611)
  parnames <- c("alpha", "beta", "gamma")
  par <- fit$par[parnames]
  expect_identical(
    ets(wineind, model = fit, alpha = 0.1611, use.initial.values = FALSE)$par[
      parnames
    ],
    par
  )
  expect_identical(
    ets(
      wineind,
      model = fit,
      alpha = 0.1611,
      beta = NA,
      use.initial.values = FALSE
    )$par[parnames],
    par
  )
  expect_identical(
    ets(
      wineind,
      model = fit,
      alpha = 0.1611,
      gamma = NA,
      use.initial.values = FALSE
    )$par[parnames],
    par
  )
  expect_identical(
    ets(
      wineind,
      model = fit,
      alpha = 0.1611,
      phi = NA,
      use.initial.values = FALSE
    )$par[parnames],
    par
  )
  expect_identical(
    ets(wineind, model = fit, alpha = 0.1611, use.initial.values = TRUE)$par,
    fit$par
  )
})

test_that("class methods for ets work", {
  fit <- ets(wineind, model = "MAM", alpha = 0.1611)
  expect_output(print(summary(fit)), "Smoothing parameters")
  expect_length(coef(fit), 16L)
  expect_lt(abs(logLik(fit) + 1802.9586023), 1e-5)
  plot(fit)
})

test_that("test ets() for errors", {
  expect_warning(ets(taylor))
  fit1 <- ets(airmiles, lambda = 0.15, biasadj = FALSE)
  expect_gt(fit1$par["alpha"], 0.95)
  fit2 <- ets(airmiles, lambda = 0.15, biasadj = TRUE)
  expect_lt(fit2$par["beta"], 1e-3)
  expect_false(identical(fit1$fitted, fit2$fitted))
  expect_error(ets(taylor, model = "ZZA"))
})

test_that("ets() fits simple models to very short series", {
  fit <- ets(ts(c(3, 5, 4, 6, 7)))
  expect_s3_class(fit, "ets")
  expect_length(forecast(fit, h = 3)$mean, 3)

  expect_identical(
    ets(ts(c(3, 5, 4, 6, 7)), model = "AAN")$method,
    "ETS(A,A,N)"
  )

  y <- ts(c(10, 12, 9, 14, 11, 13, 10, 15), frequency = 4)
  expect_identical(ets(y, model = "AAA")$method, "ETS(A,A,A)")

  expect_snapshot(fit <- ets(ts(c(3, 5, 4, 6, 7)), damped = TRUE))
})

test_that("forecast.ets()", {
  fit <- ets(airmiles, lambda = 0.15, biasadj = TRUE)
  fcast1 <- forecast(fit, PI = FALSE)
  expect_true(is.null(fcast1$upper) && is.null(fcast1$lower))
  fcast1 <- forecast(fit, biasadj = FALSE)
  fcast2 <- forecast(fit, biasadj = TRUE)
  expect_false(identical(fcast1$mean, fcast2$mean))
  fcast <- forecast(fit, simulate = TRUE)
  expect_true(!is.null(fcast$upper) && !is.null(fcast$lower))
  expect_true(all(fcast$upper > fcast$lower))
})

test_that("forecast.ets() uses phi for damped multiplicative seasonal models", {
  fit <- ets(AirPassengers, "MAM", damped = TRUE, phi = 0.8)
  # Within one seasonal period the analytic mean equals the recursive point
  # forecast used by the simulation path
  recursive <- forecast(fit, h = 12, simulate = TRUE, npaths = 5)$mean
  expect_equal(forecast(fit, h = 12)$mean, recursive)
})

test_that("class3() uses phi in the forecast variance", {
  out <- class3(
    h = 4,
    last.state = c(100, 2, 1.1, 0.9),
    trendtype = "A",
    damped = TRUE,
    m = 2,
    sigma2 = 0.01,
    par = c(alpha = 0.5, beta = 0.1, gamma = 0.1, phi = 0.8)
  )
  expect_equal(out$mu, c(91.44, 113.168, 93.57248736, 115.268400192))
  expect_equal(
    out$var,
    c(83.612736, 170.507496977664, 163.890150274293, 310.920875239663)
  )
})
