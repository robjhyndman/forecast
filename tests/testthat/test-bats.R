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
	  expect_output(print(fit), "Seed States")
	  expect_equal(length(residuals(fit)), 50L)
	  plot(fit)
	  expect_equal(bats(1, use.box.cox = TRUE, use.parallel = FALSE)$AIC, -Inf)
	  expect_equal(bats(-1, use.box.cox = TRUE, use.parallel = FALSE)$AIC, -Inf )
	  skip_on_cran()
	  skip_on_travis()
	  expect_gt(bats(abc, use.box.cox = TRUE, use.parallel = TRUE)$lambda, 0.999)
	})

}

