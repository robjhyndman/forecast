# A unit test for nnetar.R
if(require(testthat) & require(fpp))
{
	context("Testing nnetar")
	test_that("Tests for nnetar", {
		oilnnet <- nnetar(oil, lambda = 0.15)
		cafennet <- nnetar(cafe)
		expect_output(print(cafennet), regexp = "Series: cafe")
		expect_true(length(forecast(oilnnet)$mean) == 10)
		expect_true(length(forecast(cafennet)$mean) == 2 * frequency(cafe))
	})
}
