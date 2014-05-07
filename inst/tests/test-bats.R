# A unit test for bats function
if(require(testthat))
{
	context("Tests on input")
	test_that("tests for a non-ts object", {
  	set.seed(123)
  	abc <- rnorm(50, 5, 1)
  	fit <- bats(abc, use.box.cox = TRUE, use.parallel = FALSE)
  	fit$lambda
  	expect_that(fit$lambda == 0, is_false())
	})
}

