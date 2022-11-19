# A unit test for spline.R
if (require(testthat)) {
  context("Testing splinef()")
  test_that("Tests for splinef()", {
    plot.splineforecast(splinef(airmiles))
    fit1 <- splinef(woolyrnq, lambda = 0.2, biasadj = FALSE)
    fit2 <- splinef(woolyrnq, lambda = 0.2, biasadj = TRUE)
    expect_false(identical(fit1$mean, fit2$mean))
    splinef(woolyrnq, method = "mle")
    splinef(WWWusage, method = "mle")
    expect_error(splinef(woolyrnq, level = 110))
    expect_error(splinef(woolyrnq, level = -10))
  })
}
