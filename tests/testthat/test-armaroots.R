# A unit test for armaroots.R
if(require(testthat) & require(fpp))
{
	context("Testing armaroots")
	test_that("Tests for plot.Arima()", {
		arimafit <- auto.arima(ausbeer)
		expect_that(plot(arimafit), not(throws_error()))
		expect_that(plot(arimafit, type = "ma"), not(throws_error()))
		expect_that(plot(arimafit, type = "ar"), not(throws_error()))
		expect_error(plot.Arima(1:5))
		expect_error(plot(auto.arima(1:10)))
		expect_error(plot(auto.arima(1:10), type = "ar"))
		expect_error(plot.ar(1:15))
	})
}
