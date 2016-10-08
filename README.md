#forecast
[![Travis-CI Build Status](https://travis-ci.org/robjhyndman/forecast.svg?branch=master)](https://travis-ci.org/robjhyndman/forecast)
[![Coverage Status](https://coveralls.io/repos/robjhyndman/forecast/badge.svg?branch=master&service=github)](https://coveralls.io/r/robjhyndman/forecast?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/forecast)](https://cran.r-project.org/package=forecast)
[![Downloads](http://cranlogs.r-pkg.org/badges/forecast)](https://cran.r-project.org/package=forecast)
[![Rdoc](http://www.rdocumentation.org/badges/version/forecast)](http://www.rdocumentation.org/packages/forecast)
[![Pending Pull-Requests](http://githubbadges.herokuapp.com/robjhyndman/forecast/pulls.svg?style=flat)](https://github.com/robjhyndman/forecast/pulls)

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

# ETS forecasts
fit <- ets(USAccDeaths)
plot(forecast(fit))

# Automatic ARIMA forecasts
fit <- auto.arima(WWWusage)
plot(forecast(fit, h=20))

# ARFIMA forecasts
library(fracdiff)
x <- fracdiff.sim( 100, ma=-.4, d=.3)$series
fit <- arfima(x)
plot(forecast(fit, h=30))

# Forecasting with STL
tsmod <- stlm(USAccDeaths, modelfunction=ar)
plot(forecast(tsmod, h=36))

plot(stlf(AirPassengers, lambda=0))

decomp <- stl(USAccDeaths,s.window="periodic")
plot(forecast(decomp))

# TBATS forecasts
fit <- tbats(USAccDeaths)
plot(forecast(fit))

taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit))
```

## License

This package is free and open source software, licensed under GPL (>= 2).
