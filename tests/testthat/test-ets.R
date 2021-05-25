# A unit test for ets function
if (require(testthat)) {
  context("Tests on input")
  test_that("tests for some arguments in ets", {
    fit <- ets(wineind, model = "ZZM")
    comp <- paste0(fit$components[1:3], collapse = "")
    expect_that(comp, equals("MAM"))
  })

  test_that("tests for some arguments in ets", {
    fit <- ets(wineind, model = "MAM", alpha = 0.1611)
    expect_that(as.numeric(fit$par["alpha"]), equals(0.1611))
  })

  test_that("refit ets model to new data", {
    fit <- ets(wineind, model = "MAM", alpha = 0.1611)
    parnames <- c("alpha", "beta", "gamma")
    par <- fit$par[parnames]
    expect_identical(ets(wineind, model = fit, alpha = 0.1611)$par[parnames], par)
    expect_identical(ets(wineind, model = fit, alpha = 0.1611, beta = NA)$par[parnames], par)
    expect_identical(ets(wineind, model = fit, alpha = 0.1611, gamma = NA)$par[parnames], par)
    expect_identical(ets(wineind, model = fit, alpha = 0.1611, phi = NA)$par[parnames], par)
    expect_identical(ets(wineind, model = fit, alpha = 0.1611, use.initial.values = TRUE)$par, fit$par)
  })

  test_that("class methods for ets work", {
    fit <- ets(wineind, model = "MAM", alpha = 0.1611)
    expect_output(print(summary(fit)), "Smoothing parameters")
    expect_equal(length(coef(fit)), 16L)
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

  test_that("forecast.ets()", {
    fit <- ets(airmiles, lambda = 0.15, biasadj = TRUE)
    fcast1 <- forecast(fit, PI = FALSE)
    expect_true(is.null(fcast1$upper) & is.null(fcast1$lower))
    fcast1 <- forecast(fit, biasadj = FALSE)
    fcast2 <- forecast(fit, biasadj = TRUE)
    expect_false(identical(fcast1$mean, fcast2$mean))
    fcast <- forecast(fit, simulate = TRUE)
    expect_true(!is.null(fcast$upper) & !is.null(fcast$lower))
    expect_true(all(fcast$upper > fcast$lower))
  })
}
