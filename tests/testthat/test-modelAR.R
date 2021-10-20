# A unit test for modelAR.R
if (require(testthat)) {
  context("Testing modelAR")
  test_that("Tests for modelAR", {
    ## Set up functions to match 'nnetar' behavior
    avnnet2 <- function(x, y, repeats = repeats, linout = TRUE, trace = FALSE, ...) {
      mods <- list()
      for (i in 1:repeats) {
        mods[[i]] <- nnet::nnet(x, y, linout = linout, trace = trace, ...)
      }
      return(structure(mods, class = "nnetarmodels"))
    }
    ##
    predict.avnnet2 <- function(model, newdata = NULL) {
      if (is.null(newdata)) {
        if (length(predict(model[[1]])) > 1) {
          rowMeans(sapply(model, predict))
        } else {
          mean(sapply(model, predict))
        }
      } else {
        if (NCOL(newdata) >= 2 & NROW(newdata) >= 2) {
          rowMeans(sapply(model, predict, newdata = newdata))
        } else {
          mean(sapply(model, predict, newdata = newdata))
        }
      }
    }
    ## compare residuals to 'nnetar'
    expect_silent({
      set.seed(123)
      nnetar_model <- nnetar(lynx[1:100], p = 2, P = 1, size = 3, repeats = 20)
      set.seed(123)
      modelAR_model <- modelAR(lynx[1:100], FUN = avnnet2, predict.FUN = predict.avnnet2, p = 2, P = 1, scale.inputs = TRUE, size = 3, repeats = 20)
      res1 <- residuals(nnetar_model)
      res2 <- residuals(modelAR_model)
    })
    expect_true(identical(res1, res2))
    ## check re-fitting old model and compare to 'nnetar'
    expect_silent({
      nnetar_model2 <- nnetar(lynx[101:114], model = nnetar_model)
      modelAR_model2 <- modelAR(lynx[101:114], FUN = avnnet2, predict.FUN = predict.avnnet2, model = modelAR_model)
      res1 <- residuals(nnetar_model2)
      res2 <- residuals(modelAR_model2)
    })
    expect_true(identical(res1, res2))
    ## compare forecasts with 'nnetar'
    expect_silent({
      f1 <- forecast(nnetar_model)$mean
      f2 <- forecast(modelAR_model)$mean
    })
    expect_true(identical(f1, f2))
    ## test lambda and compare to 'nnetar'
    expect_silent({
      set.seed(123)
      oilnnet_nnetar <- nnetar(airmiles, lambda = 0.15, size = 1, repeats = 20)
      set.seed(123)
      oilnnet_modelAR <- modelAR(airmiles, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, lambda = 0.15, size = 1, repeats = 20)
    })
    expect_true(identical(residuals(oilnnet_nnetar, type = "response"), residuals(oilnnet_modelAR, type = "response")))
    expect_true(length(forecast(oilnnet_modelAR)$mean) == 10)
    ## check print input name
    expect_silent(woolyrnqnnet <- modelAR(woolyrnq, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, p = 1, P = 0, size = 8, repeats = 10))
    expect_output(print(woolyrnqnnet), regexp = "Series: woolyrnq")
    ## check default forecast length
    expect_true(length(forecast(woolyrnqnnet)$mean) == 2 * frequency(woolyrnq))
    #
    # Test with single-column xreg (which might be a vector)
    expect_silent({
      set.seed(123)
      woolyrnqnnet <- modelAR(woolyrnq, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, xreg = 1:length(woolyrnq), p = 2, P = 2, size = 4, repeats = 10)
      set.seed(123)
      woolyrnqnnet2 <- nnetar(woolyrnq, xreg = 1:length(woolyrnq), p = 2, P = 2, size = 4, repeats = 10)
    })
    expect_true(all(dim(woolyrnqnnet$xreg) == c(119, 1)))
    expect_true(length(forecast(woolyrnqnnet, xreg = 120:130)$mean) == 11)
    expect_true(identical(forecast(woolyrnqnnet, xreg = 120:130)$mean, forecast(woolyrnqnnet2, xreg = 120:130)$mean))
    ## Test with multiple-column xreg
    set.seed(123)
    winennet <- modelAR(wineind, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, xreg = cbind(bizdays(wineind), fourier(wineind, 1)), p = 2, P = 1, size = 4, repeats = 10)
    set.seed(123)
    winennet2 <- nnetar(
      wineind,
      xreg = cbind(bizdays(wineind), fourier(wineind, 1)),
      p = 2, P = 1, size = 4, repeats = 10
    )
    expect_true(length(forecast(winennet, h = 2, xreg = matrix(2, 2, 3))$mean) == 2L)
    ## Test if h matches xreg
    expect_true(length(forecast(winennet, h = 5, xreg = matrix(2, 2, 3))$mean) == 2L)
    expect_warning(forecast(winennet2, xreg = matrix(2, 2, 3))$mean, "different column names") %>%
      expect_equal(forecast(winennet, xreg = matrix(2, 2, 3))$mean)
    ## Test that P is ignored if m=1
    expect_warning(wwwnnet <- modelAR(WWWusage, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, xreg = 1:length(WWWusage), p = 2, P = 4, size = 3, repeats = 10))
    ## Test passing arguments to nnet
    expect_silent({
      set.seed(123)
      wwwnnet <- modelAR(WWWusage, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, xreg = 1:length(WWWusage), p = 2, P = 0, size = 3, decay = 0.1, repeats = 10)
      set.seed(123)
      wwwnnet2 <- nnetar(WWWusage, size = 3, p = 2, P = 0, xreg = 1:length(WWWusage), decay = 0.1, repeats = 10)
    })
    expect_true(identical(
      forecast(wwwnnet, h = 2, xreg = (length(WWWusage) + 1):(length(WWWusage) + 5))$mean,
      forecast(wwwnnet2, h = 2, xreg = (length(WWWusage) + 1):(length(WWWusage) + 5))$mean
    ))
    ## Test output format correct when NAs present
    airna <- airmiles
    airna[12] <- NA
    expect_warning(airnnet <- modelAR(airna, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, p = 1, size = 0, skip = TRUE, Wts = c(0, 1), maxit = 0, repeats = 5))
    expect_equal(airnnet$fitted[-c(1, 12, 13)], airna[-c(11, 12, length(airna))])
    ## Test model argument
    expect_silent({
      set.seed(123)
      fit1 <- modelAR(
        WWWusage,
        FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE,
        xreg = 1:length(WWWusage),
        p = 3, size = 2, lambda = 2, decay = 0.5, maxit = 25, repeats = 7
      )
      fit2 <- modelAR(WWWusage, xreg = 1:length(WWWusage), model = fit1)
      set.seed(123)
      fit3 <- nnetar(WWWusage, xreg = 1:length(WWWusage), p = 3, size = 2, lambda = 2, decay = 0.5, maxit = 25, repeats = 7)
    })
    # Check some model parameters
    expect_true(identical(fit1$p, fit2$p))
    expect_true(identical(fit1$lambda, fit2$lambda))
    expect_true(identical(fit1$modelargs, fit2$modelargs))
    # Check fitted values are all the same
    expect_true(identical(fitted(fit1), fitted(fit2)))
    expect_true(identical(fitted(fit1, h = 2), fitted(fit2, h = 2)))
    # Check residuals all the same
    expect_true(identical(residuals(fit1), residuals(fit2)))
    # Check number of neural nets
    expect_true(identical(length(fit1$model), length(fit2$model)))
    # Check neural network weights all the same
    expect_true(identical(fit1$model[[1]]$wts, fit2$model[[1]]$wts))
    expect_true(identical(fit1$model[[7]]$wts, fit2$model[[7]]$wts))
    ## compare results with 'nnetar'
    expect_true(identical(fitted(fit1), fitted(fit3)))
    expect_true(identical(fitted(fit1, h = 3), fitted(fit3, h = 3)))
    expect_true(identical(residuals(fit1, type = "response"), residuals(fit3, type = "response")))
    ## Check subset argument using indices
    expect_silent({
      set.seed(123)
      airnnet <- modelAR(airmiles, , FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, subset = 11:20, p = 1, size = 1, repeats = 10)
      set.seed(123)
      airnnet2 <- nnetar(airmiles, , subset = 11:20, p = 1, size = 1, repeats = 10)
    })
    expect_true(identical(which(!is.na(fitted(airnnet))), 11:20))
    expect_true(identical(fitted(airnnet), fitted(airnnet2)))
    expect_true(identical(forecast(airnnet, h = 5)$mean, forecast(airnnet2, h = 5)$mean))
    ## Check subset argument using logical vector
    expect_silent({
      set.seed(123)
      airnnet <- modelAR(airmiles, FUN = avnnet2, predict.FUN = predict.avnnet2, scale.inputs = TRUE, subset = c(rep(F, 10), rep(T, 10), rep(F, length(airmiles) - 20)), p = 1, size = 1, repeats = 10)
      set.seed(123)
      airnnet2 <- nnetar(airmiles, , subset = c(rep(F, 10), rep(T, 10), rep(F, length(airmiles) - 20)), p = 1, size = 1, repeats = 10)
    })
    expect_true(identical(which(!is.na(fitted(airnnet))), 11:20))
    expect_true(identical(fitted(airnnet), fitted(airnnet2)))
    expect_true(identical(forecast(airnnet, h = 5)$mean, forecast(airnnet2, h = 5)$mean))
    ## compare prediction intervals with 'nnetar'
    expect_silent({
      set.seed(456)
      f1 <- forecast(airnnet, h = 5, PI = TRUE, npaths = 100)
      set.seed(456)
      f2 <- forecast(airnnet2, h = 5, PI = TRUE, npaths = 100)
    })
    expect_true(identical(f1$upper, f2$upper))
    expect_true(identical(f1$lower, f2$lower))
    ## Check short and constant data
    expect_warning(nnetfit <- modelAR(rep(1, 10), FUN = avnnet2, predict.FUN = predict.avnnet2, p = 2, P = 0, size = 1, repeats = 1, lambda = 0.1), "Constant data")
    expect_true(nnetfit$p == 1)
    expect_true(is.null(nnetfit$lambda))
    expect_true(is.null(nnetfit$scalex))
    expect_error(nnetfit <- modelAR(rnorm(2), FUN = avnnet2, predict.FUN = predict.avnnet2, p = 1, P = 0, size = 1, repeats = 1), "Not enough data")
    expect_silent(nnetfit <- modelAR(rnorm(3), FUN = avnnet2, predict.FUN = predict.avnnet2, p = 1, P = 0, size = 1, repeats = 1))
    expect_true(nnetfit$p == 1)
    expect_silent(nnetfit <- modelAR(rnorm(3), FUN = avnnet2, predict.FUN = predict.avnnet2, p = 2, P = 0, size = 1, repeats = 1))
    expect_true(nnetfit$p == 2)
    expect_warning(nnetfit <- modelAR(rnorm(3), FUN = avnnet2, predict.FUN = predict.avnnet2, p = 3, P = 0, size = 1, repeats = 1), "short series")
    expect_true(nnetfit$p == 2)
    expect_warning(nnetfit <- modelAR(rnorm(3), FUN = avnnet2, predict.FUN = predict.avnnet2, p = 4, P = 0, size = 1, repeats = 1), "short series")
    expect_true(nnetfit$p == 2)
    expect_warning(nnetfit <- modelAR(rnorm(10), FUN = avnnet2, predict.FUN = predict.avnnet2, xreg = rep(1, 10), p = 2, P = 0, size = 1, repeats = 1, lambda = 0.1), "Constant xreg")
    expect_true(is.null(nnetfit$scalexreg))
    expect_warning(nnetfit <- modelAR(rnorm(3), FUN = avnnet2, predict.FUN = predict.avnnet2, xreg = matrix(c(1, 2, 3, 1, 1, 1), ncol = 2), p = 1, P = 0, size = 1, repeats = 1, lambda = 0.1), "Constant xreg")
    expect_true(is.null(nnetfit$scalexreg))
  })
}
