# A unit test for armaroots.R
if(require(testthat))
{
	context("Testing armaroots")
	test_that("Tests for plot.Arima()", {
		arimafit <- auto.arima(lynx)
		plot(arimafit)
		plot(arimafit, type = "ma")
		plot(arimafit, type = "ar")
		expect_error(plot.Arima(1:5))
		expect_error(plot(auto.arima(1:10)))
		expect_error(plot(auto.arima(1:10), type = "ar"))
		expect_error(plot.ar(1:15))
	})
}
