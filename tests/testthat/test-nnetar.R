# A unit test for nnetar.R
if(require(testthat) & require(fpp))
{
	context("Testing nnetar")
	test_that("Tests for nnetar", {
                oilnnet <- nnetar(oil, lambda = 0.15)
		cafennet <- nnetar(cafe)
		expect_output(print(cafennet), regexp = "Series: cafe")
		expect_true(length(forecast(oilnnet)$mean) == 10)
		expect_true(length(forecast(cafennet)$mean) == 2 * frequency(cafe))
                #
                # Test with single-column xreg (which might be a vector)
                uscnnet <- nnetar(usconsumption[, "consumption"],
                                  xreg=usconsumption[, "income"])
                expect_true(all(dim(uscnnet$xreg) == c(164, 1)))
                expect_true(length(forecast(
                  uscnnet, xreg=usconsumption[1:11, "income"])$mean) == 11)
                # Test default size with and without xreg
                uscnnet <- nnetar(usconsumption[, "consumption"], p=2, P=2)
                expect_output(print(uscnnet), regexp = "NNAR(2,2,2)",
                              fixed=TRUE)
                expect_output(print(uscnnet), regexp = "4-2-1 network",
                              fixed=TRUE)
                expect_true(uscnnet$size == 2)
                uscnnet <- nnetar(usconsumption[, "consumption"], p=2, P=2,
                                  xreg=usconsumption[, "income"])
                expect_output(print(uscnnet), regexp = "NNAR(2,2,3)",
                              fixed=TRUE)
                expect_output(print(uscnnet), regexp = "5-3-1 network",
                              fixed=TRUE)
                expect_true(uscnnet$size == 3)
                # Test P=0 when m>1
                uscnnet <- nnetar(usconsumption[, "consumption"], p=4, P=0)
                expect_true(uscnnet$size == 2)
                expect_output(print(uscnnet), regexp = "NNAR(4,2)", fixed=TRUE)
                # Test overlapping p & P
                uscnnet <- nnetar(usconsumption[, "consumption"], p=4, P=2)
                expect_true(uscnnet$size == 3)
                expect_output(print(uscnnet), regexp = "NNAR(4,2,3)",
                              fixed=TRUE)
                expect_output(print(uscnnet), regexp = "5-3-1 network",
                              fixed=TRUE)
                # Test with multiple-column xreg
                creditnnet <- nnetar(credit[, "score"],
                                     xreg=credit[, c("savings", "income")])
                expect_true(length(forecast(
                  creditnnet, h=3, xreg=credit[1:7, c("savings", "income")])$mean) == 7)
                # Test if h doesn't match xreg
                expect_true(length(forecast(
                  creditnnet, h=5, xreg=credit[1:7, c("savings", "income")])$mean) == 7)
                expect_true(length(forecast(
                  creditnnet, h=10, xreg=credit[1:7, c("savings", "income")])$mean) == 7)
                # Test that P is ignored if m=1
                creditnnet <- suppressWarnings(nnetar(credit[, "score"], p=2, P=4,
                                     xreg=credit[, c("savings", "income")]))
                expect_output(print(creditnnet), regexp = "NNAR(2,2)",
                              fixed=TRUE)
                # Test fixed size
                creditnnet <- nnetar(credit[, "score"], p=1, P=1, xreg=credit[, c("savings", "income")], size=12)
                expect_true(uscnnet$size == 3)
                expect_output(print(creditnnet), regexp = "NNAR(1,12)", fixed=TRUE)
                # Test passing arguments to nnet
                creditnnet <- suppressWarnings(nnetar(credit[, "score"], p=2, P=4,
                                     xreg=credit[, c("savings", "income")], decay=0.1))
                expect_output(print(creditnnet), regexp = "decay=0.1",
                              fixed=TRUE)
                ## Test output format correct
                oilnnet <- nnetar(oil, p=1, size=0, skip=TRUE, Wts=c(0, 1), maxit=0)
                expect_true(all.equal(oilnnet$fitted[-1], oil[-length(oil)]))
                ## Test output format correct when NAs present
                oilna <- oil
                oilna[12] <- NA
                suppressWarnings(oilnnet <- nnetar(oilna, p=1, size=0, skip=TRUE, Wts=c(0, 1), maxit=0))
                expect_true(all.equal(oilnnet$fitted[-c(1, 12, 13)], oilna[-c(11, 12, length(oilna))]))
                ## Test model argument
                fit1 <- nnetar(credit[, "score"],
                               xreg=credit[, c("savings", "income")],
                               lambda=2, decay=0.5, maxit=25, repeats=7)
                fit2 <- nnetar(credit[, "score"], xreg=credit[, c("savings", "income")], model=fit1)
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

	})
}
