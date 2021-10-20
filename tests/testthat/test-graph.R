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
    expect_silent(tsdisplay(airmiles, ci.type = "ma"))
    expect_silent(tsdisplay(1:20))
    expect_silent(tsdisplay(airmiles, plot.type = "scatter"))
    expect_silent(tsdisplay(airmiles, plot.type = "spectrum"))
  })
}
