# A unit test for calendar.R
if(require(testthat) & require(fpp))
{
	context("Testing calendar functions")
	test_that("Tests for bizdays()", {
		expect_error(bizdays(1:20))
		expect_that(bizdays(euretail, FinCenter = "New York"), not(throws_error()))
		expect_that(bizdays(euretail, FinCenter = "London"), not(throws_error()))
		expect_that(bizdays(h02, FinCenter = "NERC"), not(throws_error()))
		expect_that(bizdays(h02, FinCenter = "Tokyo"), not(throws_error()))
		expect_that(bizdays(euretail, FinCenter = "Zurich"), not(throws_error()))	
	})
	
	test_that("Tests for easter()", {
		expect_true(length(easter(cafe)) == length(cafe))
		expect_true(length(easter(wineind)) == length(wineind))
	})
}
