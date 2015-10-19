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
	  expect_that(ets(oil, lambda = 0.15), not(throws_error()))
	  expect_error(ets(taylor, model = "ZZA"))
	})
}
