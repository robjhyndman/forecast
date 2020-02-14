# A unit test for graph.R
if (require(testthat)) {
  context("Testing graph")
  test_that("Tests for seasonplot()", {
    expect_error(seasonplot(airmiles))
    seasonplot(ts(gold, frequency = 7))
    seasonplot(woolyrnq)
    seasonplot(wineind)
    seasonplot(wineind, year.labels = TRUE)
    seasonplot(wineind, year.labels.left = TRUE)
    # seasonplot(taylor)
  })

  test_that("Tests for tsdisplay()", {
    tsdisplay(airmiles, ci.type = "ma")
    tsdisplay(1:20)
    tsdisplay(airmiles, plot.type = "scatter")
    tsdisplay(airmiles, plot.type = "spectrum")
  })
}
