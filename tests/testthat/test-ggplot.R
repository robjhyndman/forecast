# A unit test for ggplot support
if (require(testthat)) {
  context("forecast ggplot tests")

  test_that("tests for autoplot/gg functions", {
    library(ggplot2)
    lungDeaths <- cbind(mdeaths, fdeaths)

    ggAcf(wineind)
    autoplot(Acf(wineind))
    expect_identical(ggAcf(wineind, plot = FALSE)$acf, acf(wineind, plot = FALSE, lag.max = 24)$acf)
    ggPacf(wineind)
    autoplot(Pacf(wineind))
    expect_identical(ggPacf(wineind, plot = FALSE)$acf, acf(wineind, plot = FALSE, type = "partial", lag.max = 24)$acf)
    ggCcf(mdeaths, fdeaths)
    autoplot(Ccf(mdeaths, fdeaths))
    expect_identical(ggCcf(mdeaths, fdeaths, plot = FALSE)$acf, ccf(mdeaths, fdeaths, plot = FALSE, type = "correlation", lag.max = 24)$acf)

    arimafit <- Arima(USAccDeaths, order = c(1, 1, 1), seasonal = c(1, 1, 1))
    autoplot(arimafit)
    autoplot(arimafit, type = "ma")
    autoplot(arimafit, type = "ar")

    arfit <- ar(USAccDeaths)
    autoplot(arfit)

    decomposefit <- decompose(USAccDeaths)
    autoplot(decomposefit)
    etsfit <- ets(USAccDeaths, model = "ANA")
    autoplot(etsfit)
    structfit <- StructTS(USAccDeaths)
    autoplot(structfit)
    stlfit <- stl(USAccDeaths, s.window = "periodic")
    autoplot(stlfit)

    # seasfit <- seasonal::seas(USAccDeaths)
    # autoplot(seasfit)

    etsfcast <- forecast(etsfit)
    autoplot(etsfcast)
    autoplot(etsfcast, PI = FALSE)

    lmfit <- lm(mpg ~ disp, data = mtcars)
    lmfcast <- forecast(lmfit, newdata = data.frame(disp = 214))
    autoplot(lmfcast)

    mfcast <- forecast(lungDeaths)
    autoplot(mfcast)

    ggtsdisplay(USAccDeaths, plot.type = "spectrum")
    ggtsdisplay(USAccDeaths, plot.type = "partial")
    ggtsdisplay(USAccDeaths, plot.type = "histogram")
    ggtsdisplay(USAccDeaths, plot.type = "scatter", theme = ggplot2::theme_bw())

    gglagplot(woolyrnq, lags = 2)
    gglagplot(lungDeaths, lags = 2)
    gglagplot(WWWusage, do.lines = FALSE, colour = FALSE, labels = TRUE)

    gglagchull(woolyrnq, lags = 4)

    ggmonthplot(woolyrnq)

    ggseasonplot(woolyrnq, year.labels = TRUE, year.labels.left = TRUE)
    ggseasonplot(USAccDeaths, polar = TRUE, col = 1:5, continuous = TRUE)

    splinefit <- splinef(airmiles, h = 5)
    autoplot(splinefit)

    autoplot(USAccDeaths)
    autoplot(lungDeaths)
    autoplot(lungDeaths, facets = TRUE)

    autoplot(USAccDeaths) + geom_forecast()
    autoplot(USAccDeaths) + autolayer(etsfcast, series = "ETS")
    autoplot(lungDeaths) + geom_forecast()
    autoplot(lungDeaths) + autolayer(mfcast, series = c("mdeaths", "fdeaths"))
    autoplot(lungDeaths) + autolayer(mfcast)
    autoplot(lungDeaths) + autolayer(mfcast, series = TRUE)
    autoplot(lungDeaths, facets = TRUE) + geom_forecast()

    gghistogram(USAccDeaths, add.kde = TRUE)
  })
}
