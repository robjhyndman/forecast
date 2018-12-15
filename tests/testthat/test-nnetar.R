# A unit test for nnetar.R
if (require(testthat)) {
  context("Testing nnetar")
  test_that("Tests for nnetar", {
    oilnnet <- nnetar(airmiles, lambda = 0.15)
    woolyrnqnnet <- nnetar(woolyrnq, repeats = 10)
    expect_output(print(woolyrnqnnet), regexp = "Series: woolyrnq")
    expect_true(length(forecast(oilnnet)$mean) == 10)
    expect_true(length(forecast(woolyrnqnnet)$mean) == 2 * frequency(woolyrnq))
    #
    # Test with single-column xreg (which might be a vector)
    uscnnet <- nnetar(woolyrnq, xreg = 1:length(woolyrnq))
    expect_true(all(dim(uscnnet$xreg) == c(119, 1)))
    expect_true(length(forecast(uscnnet, xreg = 120:130)$mean) == 11)
    # Test default size with and without xreg
    uscnnet <- nnetar(woolyrnq, p = 2, P = 2)
    expect_output(
      print(uscnnet), regexp = "NNAR(2,2,2)",
      fixed = TRUE
    )
    expect_output(
      print(uscnnet), regexp = "4-2-1 network",
      fixed = TRUE
    )
    expect_true(uscnnet$size == 2)
    uscnnet <- nnetar(woolyrnq, p = 2, P = 2, xreg = 1:119, repeats = 10)
    expect_output(
      print(uscnnet), regexp = "NNAR(2,2,3)",
      fixed = TRUE
    )
    expect_output(
      print(uscnnet), regexp = "5-3-1 network",
      fixed = TRUE
    )
    expect_true(uscnnet$size == 3)
    # Test P=0 when m>1
    uscnnet <- nnetar(woolyrnq, p = 4, P = 0)
    expect_true(uscnnet$size == 2)
    expect_output(print(uscnnet), regexp = "NNAR(4,2)", fixed = TRUE)
    # Test overlapping p & P
    uscnnet <- nnetar(woolyrnq, p = 4, P = 2)
    expect_true(uscnnet$size == 3)
    expect_output(
      print(uscnnet), regexp = "NNAR(4,2,3)",
      fixed = TRUE
    )
    expect_output(
      print(uscnnet), regexp = "5-3-1 network",
      fixed = TRUE
    )
    # Test with multiple-column xreg
    creditnnet <- nnetar(
      wineind,
      xreg = cbind(bizdays(wineind), fourier(wineind, 1))
    )
    expect_warning(forecast(creditnnet, h = 2, xreg = matrix(2, 2, 3))$mean, "different column names") %>% 
      expect_length(2L)
    # Test if h doesn't match xreg
    expect_warning(forecast(creditnnet, h = 5, xreg = matrix(2, 2, 3))$mean, "different column names") %>% 
      expect_length(2L)
    # Test that P is ignored if m=1
    expect_warning(creditnnet <- nnetar(WWWusage, p = 2, P = 4, xreg = 1:length(WWWusage)))
    expect_output(
      print(creditnnet), regexp = "NNAR(2,2)",
      fixed = TRUE
    )
    # Test fixed size
    creditnnet <- nnetar(WWWusage, p = 1, P = 1, xreg = 1:length(WWWusage), size = 12)
    expect_true(uscnnet$size == 3)
    expect_output(print(creditnnet), regexp = "NNAR(1,12)", fixed = TRUE)
    # Test passing arguments to nnet
    expect_warning(creditnnet <- nnetar(
      WWWusage, p = 2, P = 4,
      xreg = 1:length(WWWusage), decay = 0.1
    ))
    expect_output(
      print(creditnnet), regexp = "decay=0.1",
      fixed = TRUE
    )
    ## Test output format correct
    oilnnet <- nnetar(airmiles, p = 1, size = 0, skip = TRUE, Wts = c(0, 1), maxit = 0, repeats = 10)
    expect_true(all.equal(oilnnet$fitted[-1], airmiles[-length(airmiles)]))
    ## Test output format correct when NAs present
    oilna <- airmiles
    oilna[12] <- NA
    suppressWarnings(oilnnet <- nnetar(oilna, p = 1, size = 0, skip = TRUE, Wts = c(0, 1), maxit = 0))
    expect_true(all.equal(oilnnet$fitted[-c(1, 12, 13)], oilna[-c(11, 12, length(oilna))]))
    ## Test model argument
    fit1 <- nnetar(
      WWWusage,
      xreg = 1:length(WWWusage),
      lambda = 2, decay = 0.5, maxit = 25, repeats = 7
    )
    fit2 <- nnetar(WWWusage, xreg = 1:length(WWWusage), model = fit1)
    # Check some model parameters
    expect_true(identical(fit1$p, fit2$p))
    expect_true(identical(fit1$lambda, fit2$lambda))
    expect_true(identical(fit1$nnetargs, fit2$nnetargs))
    # Check fitted values are all the same
    expect_true(identical(fitted(fit1), fitted(fit2)))
    # Check residuals all the same
    expect_true(identical(residuals(fit1), residuals(fit2)))
    # Check number of neural nets
    expect_true(identical(length(fit1$model), length(fit2$model)))
    # Check neural network weights all the same
    expect_true(identical(fit1$model[[1]]$wts, fit2$model[[1]]$wts))
    expect_true(identical(fit1$model[[7]]$wts, fit2$model[[7]]$wts))
    # Check subset argument
    oilnnet <- nnetar(airmiles, subset = 11:20)
    expect_true(identical(which(!is.na(fitted(oilnnet))), 11:20))
    oilnnet <- nnetar(airmiles, subset = c(rep(F, 10), rep(T, 10), rep(F, length(airmiles) - 20)))
    expect_true(identical(which(!is.na(fitted(oilnnet))), 11:20))
    ## Check short and constant data
    expect_warning(nnetfit <- nnetar(rep(1, 10), p=2, P=0, size=1, repeats=1, lambda = 0.1), "Constant data")
    expect_true(nnetfit$p == 1)
    expect_true(is.null(nnetfit$lambda))
    expect_true(is.null(nnetfit$scalex))
    expect_error(nnetfit <- nnetar(rnorm(2), p=1, P=0, size=1, repeats=1), "Not enough data")
    expect_silent(nnetfit <- nnetar(rnorm(3), p=1, P=0, size=1, repeats=1))
    expect_true(nnetfit$p == 1)
    expect_silent(nnetfit <- nnetar(rnorm(3), p=2, P=0, size=1, repeats=1))
    expect_true(nnetfit$p == 2)
    expect_warning(nnetfit <- nnetar(rnorm(3), p=3, P=0, size=1, repeats=1), "short series")
    expect_true(nnetfit$p == 2)
    expect_warning(nnetfit <- nnetar(rnorm(3), p=4, P=0, size=1, repeats=1), "short series")
    expect_true(nnetfit$p == 2)
    expect_warning(nnetfit <- nnetar(rnorm(10), xreg=rep(1, 10), p=2, P=0, size=1, repeats=1, lambda = 0.1), "Constant xreg")
    expect_true(is.null(nnetfit$scalexreg))
    expect_warning(nnetfit <- nnetar(rnorm(3), xreg=matrix(c(1, 2, 3, 1, 1, 1), ncol=2), p=1, P=0, size=1, repeats=1, lambda = 0.1), "Constant xreg")
    expect_true(is.null(nnetfit$scalexreg))
  })
}
