# A unit test for arfima.R
if(require(fpp) & require(testthat))
{
	arfima1 <- arfima(austa, estim = "mle")
	arfima2 <- arfima(austa, estim = "ls")
	arfimabc <- arfima(austa, estim = "mle", lambda = 0.75)
	
	test_that("test fitted() and residuals().", {
		expect_true(all(arimaorder(arfima1) == arimaorder(arfima2)))
		fitarfima <- fitted(arfima1)
		residarfima <- residuals(arfima2)
		expect_true(length(fitarfima) == length(residarfima))
		expect_true(all(getResponse(arfima1) == austa))
	})

	test_that("test forecast.fracdiff()", {
	    expect_true(all(forecast(arfima1, fan = TRUE)$mean == forecast(arfima1, fan = FALSE)$mean))
	    expect_error(forecast(arfimabc, level = -10))
	    expect_error(forecast(arfimabc, level = 110))
	    expect_output(summary(forecast(arfimabc)), regexp = "Forecast method: ARFIMA")
	})
}
