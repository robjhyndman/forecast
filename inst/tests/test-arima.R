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
}
