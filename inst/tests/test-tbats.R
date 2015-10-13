# A unit test for tbats function
if(require(testthat) & require(fpp))
{
  context("Tests on tbats() functions")
  test_that("Test simple cases for tbats", {
    expect_error(tbats(data.frame(x1 = 1, x2 = 2), use.parallel = FALSE))
    expect_warning(tbats(c(1:5, NA, 7:9), use.parallel = FALSE))
    expect_true(all(forecast(tbats(rep(1, 100), use.parallel = FALSE))$mean == 1))
  })
  
  test_that("Test tbats() and forecasts", {
    # Fit tbats models
    tbatsfit1 <- tbats(wineind, use.parallel = FALSE)
    tbatsfit2 <- tbats(austa, use.parallel = FALSE)
    tbatsfit3 <- tbats(as.numeric(woolyrnq), seasonal.periods = frequency(woolyrnq), use.parallel = FALSE)
    tbatsfit4 <- tbats(oil, use.box.cox = FALSE, use.parallel = FALSE)
    # Test accuracy.tbats() function
    expect_output(accuracy(tbatsfit1), regexp = "ME")
    expect_output(accuracy(tbatsfit2), regexp = "ME")
    expect_output(accuracy(tbatsfit3), regexp = "ME")
    expect_output(accuracy(tbatsfit4), regexp = "ME")
    # Test summary.tbats()
    expect_output(summary(tbatsfit1), regexp = "Length")
    expect_output(summary(tbatsfit2), regexp = "Length")
    expect_output(summary(tbatsfit3), regexp = "Length")
    expect_output(summary(tbatsfit4), regexp = "Length")
    #Test fitted length
    expect_true(length(fitted(tbatsfit1)) == length(wineind))
    expect_true(length(fitted(tbatsfit2)) == length(austa))
    expect_true(length(fitted(tbatsfit3)) == length(woolyrnq))
    expect_true(length(fitted(tbatsfit4)) == length(oil))
    # Test length of forecast
    expect_true(length(forecast(tbatsfit1)$mean) == 2 * frequency(wineind))
    expect_true(length(forecast(tbatsfit2)$mean) == 10)
    #expect_true(length(forecast(tbatsfit3)$mean) == 2 * frequency(woolyrnq))
    expect_true(length(forecast(tbatsfit4)$mean) == 10)
    # Test inappropriate levels
    #expect_error(forecast(tbatsfit1, level = -10))
    #expect_error(forecast(tbatsfit1, level = 110))
    # Test forecasts with fan = TRUE
    expect_true(all(forecast(tbatsfit1, fan = TRUE)$mean == forecast(tbatsfit1)$mean))
  })
}
