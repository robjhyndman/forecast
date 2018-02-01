# A unit test for msts.R
if (require(testthat)) {
  context("Test msts.R")
  test_that("tests for msts() and print.msts()", {
    x <- msts(taylor, seasonal.periods = c(48, 336), ts.frequency = 48, start = 2000 + 22 / 52)
    expect_output(print(x), regexp = "Multi-Seasonal Time Series")
  })
}
