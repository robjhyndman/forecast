# A unit test for Arima() function
if(require(fpp) & require(testthat))
{
	context("Tests on input")
	test_that("tests for a non-ts object", {
  	set.seed(123)
  	abc <- rnorm(50, 5, 1)
  	fit <- Arima(abc, order = c(2, 0, 1))
  	expect_that(fit$arma, equals(c(2, 1, 0, 0, 1, 0, 0)))
	})

	test_that("tests for a ts with the seasonal component", {
  	fit <- Arima(a10, order = c(1, 1, 1), seasonal = c(0, 1, 1))
  	expect_that(fit$arma, equals(c(1, 1, 0, 1, 12, 1, 1)))
	})

	test_that("tests for ARIMA errors", {
  	fit <- Arima(a10, order = c(1, 1, 1), seasonal = c(0, 1, 1))
  	expect_that(arima.errors(fit), equals(a10))
	})
	
	test_that("tests for forecast.ar()", {
	fitar <- ar(taylor)
	arfc <- forecast.ar(fitar)$mean
	expect_true(all(arfc == forecast.ar(fitar, bootstrap = TRUE, npaths = 100)$mean))
	expect_true(all(arfc == forecast.ar(fitar, fan = TRUE)$mean))
	expect_error(forecast.ar(fitar, level = -10))
	expect_error(forecast.ar(fitar, level = 110))
	expect_true(all(arfc + 1 == forecast.ar(fitar, lambda = 1)$mean))
	})
	
	test_that("tests for as.character.Arima()", {
  	expect_output(as.character(auto.arima(woolyrnq, test = "adf")), regexp = "ARIMA")
	})
	
	test_that("tests for refit Arima", {
	# Refit with drift
	fitarima <- auto.arima(austa, test = "pp")
	newdata <- forecast(austa)$mean
	refitarima <- Arima(c(austa, newdata), model = fitarima)
	expect_true(all(arimaorder(fitarima) == arimaorder(refitarima)))
	# Refit seasonal model
	fitarima <- auto.arima(cafe)
	newdata <- forecast(cafe)$mean
	refitarima <- Arima(c(cafe, newdata), model = fitarima)
	expect_true(all(arimaorder(fitarima) == arimaorder(refitarima)))
	})
}
