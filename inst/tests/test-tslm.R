# A unit test for tslm function
if(require(fpp) & require(testthat))
{
  context("Tests on building model in tslm")
  test_that("tests on building model with/without data argument
            on univariate time series", {
    fit1 <- tslm(condmilk ~ trend + season, data=condmilk)
    fit2 <- tslm(condmilk ~ trend + season)
    expect_that(names(fit1), equals(names(fit2)))
    expect_that(fit1$model, equals(fit2$model))
  })

}