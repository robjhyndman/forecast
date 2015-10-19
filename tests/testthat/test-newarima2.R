# A unit test functions in newarima2.R
if(require(fpp) & require(testthat))
{
  test_that("test auto.arima() and associated methods", {
    expect_warning(auto.arima(rep(1, 100), stepwise = TRUE, parallel = TRUE))
    set.seed(345)
    testseries1 <- ts(rnorm(100) + 1:100, f = 0.1)
    xregmat <- matrix(runif(300), ncol = 3)
    expect_true(frequency(forecast(auto.arima(testseries1))) == 1)
    fit1 <- auto.arima(testseries1, xreg = xregmat, allowdrift = FALSE)
    expect_true(all(xregmat == fit1$xreg))
    
    testseries2 <- ts(rep(100, 120), f = 12)
    xregmat <- matrix(runif(240), ncol = 2)
    expect_output(auto.arima(testseries2, xreg = xregmat), regexp = "Series: testseries2")
    
    testseries3 <- 1:100
    xregmat <- testseries3 * 2
    expect_error(auto.arima(testseries3, xreg = xregmat))
    expect_output(summary(auto.arima(testseries2, xreg = xregmat, approximation = TRUE, stepwise = FALSE)), regexp = "Series: testseries2")
    expect_output(auto.arima(ts(testseries2, f = 4), approximation = TRUE, trace = TRUE), regexp = "ARIMA")
    
  })
  
  test_that("test parallel = TRUE and stepwise = FALSE for auto.arima()", {
    skip_on_travis()
    expect_output(auto.arima(austa, parallel = TRUE, stepwise = FALSE), regexp = "Call:")
  })
  
  test_that("tests for nsdiffs()", {
    expect_true(nsdiffs(wineind) == 1)
    expect_true(nsdiffs(cafe) == 1)
    expect_error(nsdiffs(oil))
    expect_true(nsdiffs(rep(1, 100)) == 0)
    expect_warning(nsdiffs(ts(rnorm(10), f = 0.1)))
  })
}
