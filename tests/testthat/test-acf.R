# A unit test for Acf() function
if(require(testthat) & require(fpp))
{
	test_that("tests for acf", {
  	out <- Acf(a10, lag.max = 10, type = "partial", plot = FALSE)
  	expect_that(length(out$lag), equals(10))
  	expect_that(out$acf, equals(Pacf(a10, lag.max = 10, plot = FALSE)$acf))
  	expect_that(Acf(a10, lag.max = 10, type = "correlation", plot = FALSE), not(throws_error()))
  	expect_that(Acf(a10, lag.max = 10, type = "correlation", plot = TRUE), not(throws_error()))
	})
}
