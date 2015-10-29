# A unit test for spline.R
if(require(testthat) & require(fpp))
{
	context("Testing splinef()")
	test_that("Tests for splinef()", {
		expect_that(plot.splineforecast(splinef(oil)), not(throws_error()))
		expect_that(splinef(cafe, lambda = 0.2), not(throws_error()))
		expect_that(splinef(cafe, method = "mle"), not(throws_error()))
		expect_that(splinef(austa, method = "mle"), not(throws_error()))
		expect_true(all(as.numeric(splinef(austa, fan = TRUE)$mean) == as.numeric(splinef(austa)$mean)))
		expect_error(splinef(cafe, level = 110))
		expect_error(splinef(cafe, level = -10))
	})
}
