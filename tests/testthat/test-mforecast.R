# A unit test for forecast.R
if (require(testthat)) {
  context("Test mforecast.R")

  mv_y <- ts(cbind(rnorm(120, 0, 3) + 1:120 + 20 * sin(2 * pi * (1:120) / 12), rnorm(120, 3, 7) + 1:120 + 16 * sin(2 * pi * (1:120 + 6) / 12)), frequency = 12)
  mv_x <- ts(cbind(rnorm(120, 0, 8) + (1:120) / 2 + 42 * sin(2 * pi * (1:120) / 12), rnorm(120, 3, 7) + (1:120) * -1 + 20 * sin(2 * pi * (1:120 + 6) / 12)), frequency = 12)
  v_y <- ts(rnorm(120, 0, 8) + (1:120) / 2 + 12 * sin(2 * pi * (1:120) / 12), frequency = 12)
  v_x <- ts(rnorm(120, 0, 1) + (1:120) * (-1) + 28 * sin(2 * pi * (1:120) / 12), frequency = 12)

  test_that("tests for is.mforecast()", {
    fit <- lm(mv_y ~ v_x)
    fcast <- forecast(fit, newdata = data.frame(v_x = 30))
    expect_true(is.mforecast(fcast))
    fit <- lm(v_y ~ v_x)
    fcast <- forecast(fit, newdata = data.frame(v_x = 30))
    expect_false(is.mforecast(fcast))
  })

  test_that("tests for mlmsplit()", {
    fit <- lm(mv_y ~ v_x)
    fit1 <- mlmsplit(fit, index = 1)
    fit2 <- mlmsplit(fit, index = 2)
    fit3 <- lm(mv_y[, 1] ~ v_x)
    fit4 <- lm(mv_y[, 2] ~ v_x)
    expect_identical(fit1$coefficients, fit3$coefficients)
    expect_identical(fit2$coefficients, fit4$coefficients)
    expect_identical(fit1$rank, fit3$rank)
    expect_identical(fit2$rank, fit4$rank)
    expect_equal(fit1$fitted.values, fit3$fitted.values)
    expect_equal(fit2$fitted.values, fit4$fitted.values)
    expect_error(mlmsplit(fit), "Must select lm")
  })

  test_that("tests for forecast.mlm()", {
    fit <- lm(mv_y ~ v_x)
    fcast <- forecast(fit, newdata = data.frame(v_x = 30))
    fit2 <- lm(mv_y[, 1] ~ v_x)
    fcast2 <- forecast(fit2, newdata = data.frame(v_x = 30))
    expect_equal(fcast$forecast[[1]]$residuals, fcast2$residuals)
  })

  test_that("tests for forecast.mts()", {
    lungDeaths <- cbind(mdeaths, fdeaths)
    fcast_b <- forecast(lungDeaths)
    fcast_m <- forecast(mdeaths)
    fcast_f <- forecast(fdeaths)
    expect_true(all.equal(fcast_b$forecast[[1]]$mean, fcast_m$mean))
    expect_true(all.equal(fcast_b$forecast[[2]]$mean, fcast_f$mean))
  })

  test_that("tests for print.mforecast()", {
    fit <- lm(mv_y ~ v_x)
    fcast <- forecast(fit, newdata = data.frame(v_x = 30))
    expect_output(print(fcast), "Series 1")
    expect_output(print(fcast), "Series 2")
  })

  test_that("tests for plot.mforecast()", {
    fit <- lm(mv_y ~ v_x)
    fcast <- forecast(fit, newdata = data.frame(v_x = 30))
    expect_silent(plot(fcast))
  })
}
