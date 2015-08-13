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
	
	test_that("functionalities and exceptions of bats", {
	  abc <- rnorm(50, 5, 1)
	  fit <- bats(abc, use.box.cox = TRUE, use.parallel = FALSE)
	  expect_that(print(fit), not(throws_error()))
	  expect_that(residuals(fit), not(throws_error()))
	  expect_that(plot(fit), not(throws_error()))
	  expect_that(bats(1, use.box.cox = TRUE, use.parallel = FALSE), not(throws_error()))
	  expect_that(bats(-1, use.box.cox = TRUE, use.parallel = FALSE), not(throws_error()))
	  skip_on_cran()
	  skip_on_travis()
	  expect_that(bats(abc, use.box.cox = TRUE, use.parallel = TRUE), not(throws_error()))
	})
	
}

