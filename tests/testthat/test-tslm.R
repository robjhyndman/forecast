# A unit test for tslm function
if(require(fpp) & require(testthat))
{
  context("Tests on building model in tslm")
  
  mv_y <- ts(cbind(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), rnorm(120,3,7) + 1:120 + 16*sin(2*pi*(1:120 + 6)/12)), frequency=12)
  mv_x <- ts(cbind(rnorm(120,0,8) + (1:120)/2 + 42*sin(2*pi*(1:120)/12), rnorm(120,3,7) + (1:120)*-1 + 20*sin(2*pi*(1:120 + 6)/12)), frequency=12)
  v_y <- ts(rnorm(120,0,8) + (1:120)/2 + 12*sin(2*pi*(1:120)/12), frequency = 12)
  v_x <- ts(rnorm(120,0,1) + (1:120)*(-1) + 28*sin(2*pi*(1:120)/12), frequency = 12)

  data <- datamat(mv_y,mv_x,v_y,v_x,fourier(v_y,3))
  
  test_that("tests on model building with univariate time series", {
    fit1 <- tslm(v_y ~ trend + season, data=data)
    fit2 <- tslm(v_y ~ trend + season, data=data, lambda=2, biasadj = FALSE)
    fit3 <- tslm(v_y ~ trend + season, data=data, lambda=2, biasadj = TRUE)
    expect_false(identical(fit2$fitted.values, fit3$fitted.values))
    fit2 <- tslm(v_y ~ trend + season, data=data.frame(trend=rnorm(120)))
    expect_false(identical(fit1$model, fit2$model))
    fit2 <- tslm(v_y ~ trend + season)
    expect_that(names(fit1), equals(names(fit2)))
    expect_that(fit1$model, equals(fit2$model))
    expect_that(fit1$coefficients, equals(fit2$coefficients))
    fit1 <- tslm(condmilk ~ trend + season, data=condmilk)
    fit2 <- tslm(condmilk ~ trend + season)
    expect_that(names(fit1), equals(names(fit2)))
    expect_that(fit1$model, equals(fit2$model))
    expect_that(fit1$coefficients, equals(fit2$coefficients))
    fit3 <- tslm(condmilk ~ trend + season, data=condmilk, subset = subset(condmilk, month=c("January", "FEB")))
    fit4 <- tslm(condmilk ~ trend + season, subset = subset(condmilk, month=c("January", "FEB")))
    expect_that(names(fit3), equals(names(fit4)))
    expect_that(fit3$model, equals(fit4$model))
    expect_that(fit3$coefficients, equals(fit4$coefficients))
    expect_false(isTRUE(all.equal(fit2$model, fit4$model)))
    expect_false(isTRUE(all.equal(fit2$coefficients, fit4$coefficients)))
    fit <- tslm(condmilk ~ trend + season + trend*season, data=condmilk)
    expect_that(fit3$coefficients, equals(fit4$coefficients))
    expect_true("trend:season"%in%attr(fit$terms,"term.labels"))
  })
  
  test_that("function renaming for forecast.lm()", {
    fit1 <- tslm(v_y ~ v_x + trend + fourier(v_y, 3), data=data.frame(v_y=v_y))
    expect_equal(grep("FN.fourier_", names(fit1$coefficients)), 4:9)
  })
  
  test_that("tests on subsetting data", {
    a <- mv_y[,1]
    expect_warning(fit1 <- tslm(mv_y ~ trend, subset=a<20), "Subset has been assumed contiguous")
    expect_warning(fit2 <- tslm(mv_y ~ trend, subset=subset(mv_y,mv_y[,1]<20)), "Subset has been assumed contiguous")
    expect_that(names(fit1), equals(names(fit2)))
    expect_that(fit1$model, equals(fit2$model))
    expect_that(fit1$coefficients, equals(fit2$coefficients))
    expect_warning(tslm(v_y ~ trend + season + trend*season, subset = subset(v_y, subset=v_y<100)), "Subset has been assumed contiguous")
    expect_error(tslm(v_y ~ trend + season + trend*season, subset = subset(v_y, month="January")), "Non-seasonal data cannot be modelled using a seasonal factor")
    fit1 <- tslm(v_y ~ trend + season + trend*season, subset = subset(v_y, month=c("January","Feb")))
    expect_true(tsp(fit1$x)[3]==2)
  })
  
  test_that("tests on model building with multivariate time series", {
    fit1 <- tslm(mv_y ~ trend + season)
    fit2 <- tslm(mv_y ~ trend + season, lambda=0.5)
    expect_false(identical(fit1$coefficients, fit2$coefficients))
    fit3 <- tslm(mv_y ~ trend + season, lambda=0.5, biasadj = TRUE)
    expect_false(identical(fit2$fitted.values, fit3$fitted.values))
    fit2 <- tslm(mv_y ~ trend + season, data=data)
    expect_that(names(fit1), equals(names(fit2)))
    expect_that(fit1$model, equals(fit2$model))
    expect_that(fit1$coefficients, equals(fit2$coefficients))
    expect_warning(fit3 <- tslm(mv_y ~ trend + season, subset = mv_y[,1]<1), "Subset has been assumed contiguous")
    expect_warning(fit4 <- tslm(mv_y ~ trend + season, data=data, subset = subset(mv_y, mv_y[,1]<1)), "Subset has been assumed contiguous")
    expect_that(names(fit3), equals(names(fit4)))
    expect_that(fit3$model, equals(fit4$model))
    expect_that(fit3$coefficients, equals(fit4$coefficients))
  })
  
  test_that("tests with bad input", {
    expect_error(tslm(mpg ~ cyl,data=mtcars), "Not time series data")
    expect_error(tslm(tmp2 ~ trend + season + trend*season, subset = subset(tmp2, month="January"), "Non-seasonal data cannot be modelled using a seasonal factor"))
  })

  test_that("forecast.lm", {
    fit1 <- tslm(v_y ~ trend + season, lambda=2, biasadj = FALSE)
    fit2 <- tslm(v_y ~ trend + season, lambda=2, biasadj = TRUE)
    fcast1 <- forecast(fit1, h=60, biasadj=FALSE)
    fcast2 <- forecast(fit2, h=60, biasadj=TRUE)
    expect_false(identical(fcast1$mean, fcast2$mean))
    })
}