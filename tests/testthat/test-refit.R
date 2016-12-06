# A unit test for re-fitting models
if(require(testthat))
{
  context("Re-fitting models")

  test_that("tests for re-fitting models", {
    #arfima
    fit <- arfima(mdeaths)
    refit <- arfima(fdeaths, model=fit)
    expect_true(identical(fit$ar, refit$ar))
    expect_true(identical(fit$ma, refit$ma))
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))
  })
}
