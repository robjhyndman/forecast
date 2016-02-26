# A unit test for ets function
if(require(fpp) & require(testthat))
{
	context("Tests on input")
	test_that("tests for some arguments in ets", {
	  fit <- ets(a10, model = "ZZM")
	  comp <- paste0(fit$components[1:3], collapse = "")
	  expect_that(comp, equals("MAM"))
	})

	test_that("tests for some arguments in ets", {
	  fit <- ets(a10, model = "MAM", alpha = 0.1611)
	  expect_that(as.numeric(fit$par["alpha"]), equals(0.1611))
	})
	
	test_that("refit ets model to new data", {
	  fit <- ets(a10, model = "MAM", alpha = 0.1611)
	  expect_that(ets(a10, model=fit, alpha=0.1611), not(throws_error()))
	  expect_that(ets(a10, model=fit, alpha=0.1611, beta=NA), not(throws_error()))
	  expect_that(ets(a10, model=fit, alpha=0.1611, gamma=NA), not(throws_error()))
	  expect_that(ets(a10, model=fit, alpha=0.1611, phi=NA), not(throws_error()))
	  expect_that(ets(a10, model=fit, alpha=0.1611, use.initial.values=TRUE), not(throws_error()))
	})
	
	test_that("class methods for ets work", {
	  fit <- ets(a10, model = "MAM", alpha = 0.1611)
	  expect_that(summary(fit), not(throws_error()))
	  expect_that(coef(fit), not(throws_error()))
	  expect_that(logLik(fit), not(throws_error()))
	  expect_that(plot(fit), not(throws_error()))
	})
	
	test_that("test ets() for errors", {
	  expect_warning(ets(taylor))
	  expect_that(fit1<-ets(oil, lambda = 0.15, biasadj=FALSE), not(throws_error()))
	  expect_that(fit2<-ets(oil, lambda = 0.15, biasadj = TRUE), not(throws_error()))
	  expect_false(identical(fit1$fitted, fit2$fitted))
	  expect_error(ets(taylor, model = "ZZA"))
	})
	
	test_that("forecast.ets()", {
	  expect_that(fit<-ets(oil, lambda = 0.15, biasadj = TRUE), not(throws_error()))
	  expect_that(fcast1 <- forecast(fit, PI=FALSE), not(throws_error()))
	  expect_true(is.null(fcast1$upper) & is.null(fcast1$lower))
	  expect_that(fcast1 <- forecast(fit, biasadj=FALSE), not(throws_error()))
	  expect_that(fcast2 <- forecast(fit, biasadj=TRUE), not(throws_error()))
	  expect_false(identical(fcast1$mean, fcast2$mean))
	  expect_that(fcast <- forecast(fit, simulate=TRUE), not(throws_error()))
	  expect_true(!is.null(fcast$upper) & !is.null(fcast$lower))
	})
}
