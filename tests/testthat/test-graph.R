# A unit test for graph.R
if(require(testthat) & require(fpp))
{
	context("Testing graph")
	test_that("Tests for seasonplot()", {
		expect_error(seasonplot(oil))
		expect_that(seasonplot(ts(gold, f = 7)), not(throws_error()))
		expect_that(seasonplot(cafe), not(throws_error()))
		expect_that(seasonplot(wineind), not(throws_error()))
		expect_that(seasonplot(wineind, year.labels = TRUE), not(throws_error()))
		expect_that(seasonplot(wineind, year.labels.left = TRUE), not(throws_error()))
		expect_that(seasonplot(taylor), not(throws_error()))
	})
		
	test_that("Tests for tsdisplay()", {
		expect_that(tsdisplay(oil, ci.type = "ma"), not(throws_error()))
		expect_that(tsdisplay(1:20), not(throws_error()))
		expect_that(tsdisplay(oil, plot.type = "scatter"), not(throws_error()))
		expect_that(tsdisplay(oil, plot.type = "spectrum"), not(throws_error()))
	})
}
