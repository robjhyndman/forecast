# A unit test for Acf() function
if (require(testthat)) {
  test_that("tests for acf", {
    out <- Acf(wineind, lag.max = 10, type = "partial", plot = FALSE)
    expect_that(length(out$lag), equals(10))
    expect_that(out$acf, equals(Pacf(wineind, lag.max = 10, plot = FALSE)$acf))
    expect_equal(dim(Acf(wineind, lag.max = 10, type = "correlation", plot = FALSE)$acf), c(11L, 1L, 1L))
    expect_equal(Acf(wineind, lag.max = 10, type = "correlation", plot = TRUE)$acf[1, 1, 1], 1)
  })
}
