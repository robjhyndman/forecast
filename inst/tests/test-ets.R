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
}