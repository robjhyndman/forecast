# A unit test for na.interp() and tsclean()
if(require(fpp) & require(testthat))
{
  test_that("tests for monthdays", {
  expect_error(monthdays(rnorm(10)))
  expect_error(monthdays(rnorm(10)))
  expect_true(all(monthdays(ts(rep(100, 12), f = 12)) == c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)))
  expect_true(all(monthdays(ts(rep(1, 4), f = 4)) == c(90, 91, 92, 92)))
  # Test leapyears
  expect_true(monthdays(ts(rep(1, 48), f = 12))[38] == 29)
  expect_true(monthdays(ts(rep(1, 16), f = 4))[13] == 91)
  })

  test_that("tests for seasonaldummy",{
  expect_error(seasonaldummy(1))
  testseries <- ts(rep(1:7, 5), f = 7)
  dummymat <- seasonaldummy(testseries)
  expect_true(length(testseries) == nrow(dummymat))
  expect_true(ncol(dummymat) == 6)
  expect_true(all(seasonaldummy(wineind)[1:11, ] == diag(11)))
  })

  test_that("tests for seasonaldummyf", {
  expect_error(seasonaldummyf(1))
  expect_warning(dummymat <- seasonaldummyf(wineind, 4), "deprecated")
  expect_true(nrow(dummymat) == 4)
  expect_true(ncol(dummymat) == 11)
  })

  test_that("tests for fourier",{
    expect_error(fourier(1))
    testseries <- ts(rep(1:7, 5), f = 7)
    fouriermat <- fourier(testseries, 3)
    expect_true(length(testseries) == nrow(fouriermat))
    expect_true(ncol(fouriermat) == 6)
    expect_true(all(grep("-7", colnames(fouriermat))))
  })

  test_that("tests for fourierf", {
    expect_error(fourierf(1))
    expect_warning(fouriermat <- fourierf(wineind, 4, 10), "deprecated")
    expect_true(nrow(fouriermat) == 10)
    expect_true(ncol(fouriermat) == 8)
  })

  test_that("tests for stlm",{
   expect_warning(stlm(ts(rep(5, 24), f = 4), etsmodel = "ZZZ"))
  })

  test_that("tests for forecast.stlm",{
  expect_error(forecast.stlm(stlm(a10), newxreg = matrix(rep(1, 24), ncol = 2)))
  stlmfit1 <- stlm(cafe, method = "ets")
  stlmfit2 <- stlm(cafe, method = "arima", approximation = FALSE)
  fcfit1 <- forecast(stlmfit1)
  fcfit2 <- forecast(stlmfit1, fan = TRUE)
  expect_true(all(fcfit2$level == seq(from = 51, to = 99, by=3)))
  fcstlmfit3 <- forecast(stlmfit2)
  expect_true(all(round(forecast(stlm(ts(rep(100, 120), f = 12)))$mean, 10) == 100))
  expect_true(all(round(forecast(stlm(ts(rep(100, 120), f = 12), lambda = 1))$mean, 10) == 100))
  })

  test_that("tests for stlf",{
  expect_true(all(forecast.stlm(stlm(wineind))$mean == stlf(wineind)$mean))
  expect_true(all(forecast.stlm(stlm(wineind, lambda = .5))$mean == stlf(wineind, lambda = .5)$mean))
  fit1 <- stlf(wineind,lambda=.2,biasadj=FALSE)
  fit2 <- stlf(wineind,lambda=.2,biasadj=TRUE)
  expect_false(identical(fit1$mean, fit2$mean))
  })

  test_that("tests for ma",{
  testseries <- ts(1:20, f = 4)
  expect_true(frequency(ma(testseries, order = 4)) == frequency(testseries))
  maseries <- ma(testseries, order = 3)
  expect_true(identical(which(is.na(maseries)), c(1L,20L)))
  expect_true(all(abs(maseries[2:19] - 2:19) < 1e-14))
  maseries <- ma(testseries,order=2,centre=FALSE)
  expect_true(identical(which(is.na(maseries)), 20L))
  expect_true(all(abs(maseries[1:19] - 1:19 - 0.5) < 1e-14))
  maseries <- ma(testseries,order=2,centre=TRUE)
  expect_true(identical(which(is.na(maseries)), c(1L,20L)))
  expect_true(all(abs(maseries[2:19] - 2:19) < 1e-14))
  })
}
