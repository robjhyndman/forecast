# A unit test for dshw function
if (require(testthat)) {
  context("Tests on dshw()")
  test_that("Test dshw()", {
    # Test negative values and period1 and period2 not specified
    set.seed(345)
    expect_error(dshw(-10:10))
    expect_error(dshw(abs(rnorm(100))))
    # Test fits with period1 and period2 swapped
    set.seed(5555)
    t <- seq(0, 1, by = 0.1)
    x <- exp(sin(2 * pi * t) + cos(2 * pi * t * 4) + rnorm(length(t), 0, 0.1))
    fit1 <- dshw(x, period1 = 4, period2 = 2)$mean
    fit2 <- dshw(x, period1 = 2, period2 = 4)$mean
    expect_true(all(fit1 == fit2))
    # Test fits with lambda specified and armethod = FALSE
    y <- x + 1
    fit3 <- dshw(y, period1 = 2, period2 = 4, lambda = 2, biasadj = FALSE)
    fit4 <- dshw(y, period1 = 2, period2 = 4, lambda = 2, biasadj = TRUE)
    expect_false(identical(fit3$mean, fit4$mean))
    fit5 <- dshw(x, period1 = 2, period2 = 4, armethod = FALSE)
    # Test fits with inappropriate periods specified
    expect_error(dshw(x, period1 = 2, period2 = 2))
    expect_error(dshw(x, period1 = 2, period2 = 4.1))
  })
}
