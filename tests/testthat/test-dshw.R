# A unit test for dshw function
if(require(testthat) & require(fpp))
{
  context("Tests on dshw()")
  test_that("Test dshw()", {
    # Test negative values and period1 and period2 not specified
    set.seed(345)
    expect_error(dshw(-10:10))
    expect_error(dshw(abs(rnorm(100))))
    # Test refit
    dshwfit <- dshw(taylor[1:682], period1 = 336, period2 = 48)
    dshwfit2 <- dshw(taylor[1:700], period1 = 336, period2 = 48, model = dshwfit)
    expect_true(all(fitted(dshwfit2)[1:682] == fitted(dshwfit)))
    # Test fits with period1 and period2 swapped
    set.seed(5555)
    t <- seq(0, 2, by= 0.05)
    x <- exp(sin(2 * pi * t) + cos(2 * pi * t * 4) + rnorm(length(t), 0, 0.1))
    fit1 <- dshw(x, period1 = 20, period2 = 5)$mean
    fit2 <- dshw(x, period1 = 5, period2 = 20)$mean
    expect_true(all(fit1 == fit2))
    # Test fits with lambda specified and armethod = FALSE
    y <- x + 1
    expect_that(dshw(y, period1 = 5, period2 = 20, lambda = 1.0), not(throws_error()))
    expect_that(dshw(x, period1 = 5, period2 = 20, armethod = FALSE), not(throws_error()))
    # Test fits with inappropriate periods specified
    expect_error(dshw(x, period1 = 5, period2 = 5))
    expect_error(dshw(x, period1 = 5, period2 = 20.1))
  })
}











