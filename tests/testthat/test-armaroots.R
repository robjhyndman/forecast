# A unit test for armaroots.R
if (require(testthat)) {
  context("Testing armaroots")
  test_that("Tests for plot.Arima()", {
    arimafit <- Arima(lynx, c(2, 0, 2), include.mean = FALSE)
    plot(arimafit)
    plot(arimafit, type = "ma")
    plot(arimafit, type = "ar")
    expect_warning(plot(Arima(lynx, c(0, 1, 0))))
  })
}
