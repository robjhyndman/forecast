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
  	expect_that(dim(accuracy(fcasts)), equals(c(1, 6)))
  	expect_that(dim(accuracy(fcasts, test)), equals(c(2, 8)))
  	expect_that(all(accuracy(fcasts, test, test = 1:2) == accuracy(fcasts, test)),
              is_false())
	})
}
