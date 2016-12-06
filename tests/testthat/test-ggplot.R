# A unit test for ggplot support
if(require(testthat))
{
  context("forecast ggplot tests")

  test_that("tests for autoplot/gg functions", {
    ldeaths <- cbind(mdeaths, fdeaths)
    
    ggAcf(wineind)
    autoplot(Acf(wineind))
    identical(ggAcf(wineind, plot=FALSE)$acf, acf(wineind, plot=FALSE, lag.max = 24)$acf)
    ggPacf(wineind)
    autoplot(Pacf(wineind))
    identical(ggPacf(wineind, plot=FALSE)$acf, acf(wineind, plot=FALSE, type="partial", lag.max = 24)$acf)
    ggCcf(mdeaths, fdeaths)
    autoplot(Ccf(mdeaths, fdeaths))
    identical(ggCcf(mdeaths, fdeaths, plot=FALSE)$acf, ccf(mdeaths, fdeaths, plot=FALSE, type="correlation", lag.max = 24)$acf)
    
    arimafit <- Arima(USAccDeaths, order = c(1,1,1), seasonal = c(1,1,1))
    autoplot(arimafit)
    autoplot(arimafit, type = "ma")
    autoplot(arimafit, type = "ar")
    
    arfit <- ar(USAccDeaths)
    autoplot(arfit)
    
    decomposefit <- decompose(USAccDeaths)
    autoplot(decomposefit)
    etsfit <- ets(USAccDeaths, model="ANA")
    autoplot(etsfit)
    
    stlfit <- stl(USAccDeaths, s.window = "periodic")
    autoplot(stlfit)
    
    seasfit <- seasonal::seas(USAccDeaths)
    autoplot(seasfit)
    
    etsfcast <- forecast(etsfit)
    autoplot(etsfcast)
    autoplot(etsfcast, plot.conf = FALSE)
    
    lmfit <- lm(mpg ~ disp, data=mtcars)
    lmfcast <- forecast(lmfit, newdata=data.frame(disp=214))
    autoplot(lmfcast)
    
    mfcast <- forecast(ldeaths)
    autoplot(mfcast)
    
    ggtsdisplay(USAccDeaths, plot.type = "spectrum")
    ggtsdisplay(USAccDeaths, plot.type = "partial")
    ggtsdisplay(USAccDeaths, plot.type = "histogram")
    ggtsdisplay(USAccDeaths, plot.type = "scatter", theme=ggplot2::theme_bw())
    
    gglagplot(woolyrnq, lags=2)
    gglagplot(ldeaths, lags=2)
    gglagplot(WWWusage, do.lines = FALSE, colour = FALSE, labels = TRUE)
    
    gglagchull(woolyrnq, lags=4)
    
    ggmonthplot(woolyrnq)
    
    ggseasonplot(woolyrnq, year.labels = TRUE, year.labels.left = TRUE)
    ggseasonplot(USAccDeaths, polar=TRUE, col=1:5, continuous = TRUE)
    
    splinefit <- splinef(airmiles, h=5)
    autoplot(splinefit)
    
    autoplot(USAccDeaths)
    autoplot(ldeaths)
    autoplot(ldeaths, facet=TRUE)
    
    autoplot(USAccDeaths) + geom_forecast()
    autoplot(USAccDeaths) + geom_forecast(etsfcast, series="ETS")
    autoplot(ldeaths) + geom_forecast()
    autoplot(ldeaths, facet=TRUE) + geom_forecast()
    
    gghistogram(USAccDeaths, add.kde = TRUE)
  })
}
