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
                creditnnet <- nnetar(credit[, "score"], p=2, P=4,
                                     xreg=credit[, c("savings", "income")])
                expect_output(print(creditnnet), regexp = "NNAR(2,2)",
                              fixed=TRUE)
                # Test fixed size
                creditnnet <- nnetar(credit[, "score"], p=1, P=1, xreg=credit[, c("savings", "income")], size=12)
                expect_true(uscnnet$size == 3)
                expect_output(print(creditnnet), regexp = "NNAR(1,12)", fixed=TRUE)
                # Test passing arguments to nnet
                creditnnet <- nnetar(credit[, "score"], p=2, P=4,
                                     xreg=credit[, c("savings", "income")], decay=0.1)
                expect_output(print(creditnnet), regexp = "decay=0.1",
                              fixed=TRUE)

	})
}
