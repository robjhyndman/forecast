# A unit test for accuracy() function
if(require(fpp) & require(testthat))
{
	context("Tests on input")
	test_that("tests for a non-forecast object", {
  	expect_that(accuracy(a10), throws_error())
	})

	context("Tests on output")
	test_that("tests for dimension", {
  	train <- window(a10, start = c(1991, 7), end = c(2007, 12))
  	test <- window(a10, start = c(2008, 1))
  	fcasts <- forecast(train, h = 6)
  	expect_that(dim(accuracy(fcasts)), equals(c(1, 7)))
  	expect_that(dim(accuracy(fcasts, test)), equals(c(2, 8)))
  	expect_that(all(dim(accuracy(fcasts, test, test = 1:2)) == dim(accuracy(fcasts, test))),
              is_false())
    expect_that(accuracy(fcasts, test = 1:198), equals(accuracy(fcasts)))
	})
	test_that("tests for accuracy",{
	# Test arima
	fitarima <- auto.arima(austa)
	accuracyarima <- accuracy(fitarima)[1, "RMSE"]
	accuracyarimasim <- accuracy(auto.arima(simulate(fitarima, seed = 123)))[1, "RMSE"]
	expect_more_than(accuracyarima, accuracyarimasim)
	# Test ets
	fitets <- ets(austa)
	accuracyets <- accuracy(fitets)[1, "RMSE"]
	accuracyetssim <- accuracy(ets(simulate(fitets, seed = 123)))[1, "RMSE"]
	expect_more_than(accuracyets, accuracyetssim)
	# Test lm
	month <- factor(rep(1:12, 14))
	fitlm <- lm(wineind[1:168] ~ month)
	accuracylm <- accuracy(fitlm)[1, "RMSE"]
	accuracylmsim <- accuracy(lm(simulate(fitlm, seed = 123)[, 1] ~ month))[1, "RMSE"]
	expect_more_than(accuracylm, accuracylmsim)
	})
}
