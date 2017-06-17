forecast <img src="man/figures/logo.png" align="right" />
======================

[![Travis-CI Build Status](https://travis-ci.org/robjhyndman/forecast.svg?branch=master)](https://travis-ci.org/robjhyndman/forecast)
[![Coverage Status](https://coveralls.io/repos/robjhyndman/forecast/badge.svg?branch=master&service=github)](https://coveralls.io/r/robjhyndman/forecast?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/forecast)](https://cran.r-project.org/package=forecast)
[![Downloads](http://cranlogs.r-pkg.org/badges/forecast)](https://cran.r-project.org/package=forecast)
[![Rdoc](http://www.rdocumentation.org/badges/version/forecast)](http://www.rdocumentation.org/packages/forecast)

The R package *forecast* provides methods and tools for displaying and analysing univariate time series forecasts including exponential smoothing via state space models and automatic ARIMA modelling.

## Installation
You can install the **stable** version on
[R CRAN](https://cran.r-project.org/package=forecast).

```s
install.packages('forecast', dependencies = TRUE)
```

You can install the **development** version from
[Github](https://github.com/robjhyndman/forecast)

```s
# install.packages("devtools")
devtools::install_github("robjhyndman/forecast")
```

## Usage

```s
library(forecast)
library(ggplot2)

# ETS forecasts
USAccDeaths %>%
  ets %>%
  forecast %>%
  autoplot

# Automatic ARIMA forecasts
WWWusage %>%
  auto.arima %>%
  forecast(h=20) %>%
  autoplot

# ARFIMA forecasts
library(fracdiff)
x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
arfima(x) %>%
  forecast(h=30) %>%
  autoplot

# Forecasting with STL
USAccDeaths %>%
  stlm(modelfunction=ar) %>%
  forecast(h=36) %>%
  autoplot

AirPassengers %>%
  stlf(lambda=0) %>%
  autoplot

USAccDeaths %>%
  stl(s.window='periodic') %>%
  forecast %>%
  autoplot

# TBATS forecasts
USAccDeaths %>%
  tbats %>%
  forecast %>%
  autoplot

taylor %>%
  tbats %>%
  forecast %>%
  autoplot
```

## For more information

  * Get started in forecasting with the online textbook at http://OTexts.org/fpp2/
  * Read the Hyndsight blog at https://robjhyndman.com/hyndsight/
  * Ask forecasting questions on http://stats.stackexchange.com/tags/forecasting
  * Ask R questions on http://stackoverflow.com/tags/forecasting+r
  * Join the International Institute of Forecasters: http://forecasters.org/

## License

This package is free and open source software, licensed under GPL (>= 3).
