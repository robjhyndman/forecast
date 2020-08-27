# A unit test for Arima() function
if (require(testthat)) {
  context("Tests on input")
  test_that("tests for a non-ts object", {
    set.seed(123)
    abc <- rnorm(50, 5, 1)
    fit <- Arima(abc, order = c(2, 0, 1))
    expect_that(fit$arma, equals(c(2, 1, 0, 0, 1, 0, 0)))
  })

  test_that("tests for a ts with the seasonal component", {
    fit <- Arima(wineind, order = c(1, 1, 1), seasonal = c(0, 1, 1))
    expect_that(fit$arma, equals(c(1, 1, 0, 1, 12, 1, 1)))
  })

  test_that("tests for ARIMA errors", {
    fit <- Arima(wineind, order = c(1, 1, 1), seasonal = c(0, 1, 1))
    expect_that(residuals(fit, type = "regression"), equals(wineind))
  })

  test_that("tests for arimaorder", {
    for (ar in 1:5) {
      for (i in 0:1) {
        for (ma in 1:5) {
          fitarima <- Arima(lynx, order = c(ar, i, ma), method = "ML", include.constant = TRUE, lambda = 0.5)
          arextracted <- fitarima$arma[1]
          iextracted <- fitarima$arma[6]
          maextracted <- fitarima$arma[2]
          expect_true(all(arimaorder(fitarima) == c(arextracted, iextracted, maextracted)))
          expect_true(all(names(arimaorder(fitarima)) == c("p", "d", "q")))
          expect_true(arimaorder(fitarima)["p"] == ar)
          expect_true(arimaorder(fitarima)["d"] == i)
          expect_true(arimaorder(fitarima)["q"] == ma)
        }
      }
    }

    # Test ar
    arMod <- ar(lynx, order.max = 2)
    expect_true(arimaorder(arMod)["p"] == 2)
    expect_true(arimaorder(arMod)["d"] == 0)
    expect_true(arimaorder(arMod)["q"] == 0)
    expect_true(all(names(arimaorder(arMod)) == c("p", "d", "q")))

    # Test SARIMA
    sarimaMod <- Arima(wineind, order = c(1, 1, 2), seasonal=c(0, 1,1))
    expect_true(all(names(arimaorder(sarimaMod)) == c("p", "d", "q", "P", "D", "Q", "Frequency")))
    expect_true(arimaorder(sarimaMod)["p"] == 1)
    expect_true(arimaorder(sarimaMod)["d"] == 1)
    expect_true(arimaorder(sarimaMod)["q"] == 2)
    expect_true(arimaorder(sarimaMod)["P"] == 0)
    expect_true(arimaorder(sarimaMod)["D"] == 1)
    expect_true(arimaorder(sarimaMod)["Q"] == 1)
    expect_true(arimaorder(sarimaMod)["Frequency"] == frequency(wineind))

    # Test fracdiff
    set.seed(4)
    fracdiffMod <- fracdiff::fracdiff(lynx, nar = 2)
    expect_true(all(names(arimaorder(fracdiffMod)) == c("p", "d", "q")))
    expect_true(arimaorder(fracdiffMod)["p"] == 2)
    expect_true(arimaorder(fracdiffMod)["d"] >= 0)
    expect_true(arimaorder(fracdiffMod)["d"] <= 1)
    expect_true(arimaorder(fracdiffMod)["p"] == 2)
  })

  test_that("tests for forecast.Arima", {
    fit1 <- Arima(wineind, order = c(1, 1, 2), seasonal = c(0, 1, 1), method = "CSS")
    expect_warning(forecast.Arima(fit1, xreg = 1:10), "xreg not required")
    expect_warning(forecast.Arima(fit1, include.drift = TRUE))
    expect_true(all.equal(forecast.Arima(fit1, bootstrap = TRUE, npaths = 100)$ mean, forecast.Arima(fit1)$mean))

    fit2 <- Arima(wineind, order = c(1, 0, 1), seasonal = c(0, 0, 0), include.drift = TRUE)
    expect_warning(Arima(wineind, order = c(1, 2, 1), include.drift = TRUE))
    expect_true("drift" %in% names(coef(fit2)))
    expect_true(length(forecast.Arima(fit2)$mean) == 2 * frequency(wineind))

    fit3 <- Arima(wineind, order = c(1, 1, 2), seasonal = c(0, 1, 1), include.mean = FALSE)
    expect_false("intercept" %in% names(coef(fit3)))
    expect_true(frequency(forecast.Arima(fit3)$mean) == frequency(wineind))

    fit4 <- Arima(wineind, order = c(1, 1, 2), seasonal = c(0, 1, 1), xreg = rnorm(length(wineind)))
    expect_error(forecast.Arima(fit4))
    expect_error(forecast.Arima(fit4, xreg = matrix(rnorm(40), ncol = 2)))
    forecast.Arima(fit4, xreg = rnorm(20))$mean %>% 
      length %>% 
      expect_equal(20)

    fit5 <- Arima(wineind[1:150], order = c(1, 1, 2), seasonal = c(0, 1, 1), method = "ML")
    expect_true(accuracy(fit5)[1, "MAPE"] < accuracy(Arima(wineind, model = fit5))[1, "MAPE"])

    fit6 <- Arima(wineind, order = c(1, 1, 2), seasonal = c(0, 1, 1), method = "CSS", lambda = 5)
    expect_false(identical(fit1$coef, fit6$coef))
  })

  test_that("tests for search.arima", {
    set.seed(444)
    arimasim <- arima.sim(n = 300, model = list(ar = runif(8, -.1, 0.1), ma = runif(8, -0.1, 0.1), sd = 0.1))
    expect_true(AIC(auto.arima(arimasim)) >= AIC(auto.arima(arimasim, stepwise = FALSE)))
  })

  test_that("tests for forecast.ar()", {
    fitar <- ar(taylor)
    arfc <- forecast.ar(fitar)$mean
    expect_true(all(arfc == forecast.ar(fitar, bootstrap = TRUE, npaths = 100)$mean))
    expect_true(all(arfc == forecast.ar(fitar, fan = TRUE)$mean))
    expect_error(forecast.ar(fitar, level = -10))
    expect_error(forecast.ar(fitar, level = 110))
    expect_true(all(arfc + 1 == forecast.ar(fitar, lambda = 1)$mean))
    arfcbc <- forecast.ar(fitar, lambda = 2)
    arfcabc <- forecast.ar(fitar, lambda = 2, biasadj = TRUE)
    expect_false(identical(arfcbc$mean, arfcabc$mean))
  })

  test_that("tests for as.character.Arima()", {
    expect_match(as.character(auto.arima(woolyrnq)), regexp = "ARIMA")
  })
}
