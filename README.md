#forecast

The R package *forecast* provides methods and tools for displaying and analysing univariate time series forecasts including exponential smoothing via state space models and automatic ARIMA modelling.

## Installation
You can install the **stable** version on 
[R CRAN](http://cran.r-project.org/package=forecast).

```s
install.packages('forecast', dependencies = TRUE)
```

You can also install the **development** version from
[Github](https://github.com/robjhyndman/forecast)

```s
# install.packages("devtools")
library(devtools)
install_github("robjhyndman/forecast") 
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
fit <- tbats(USAccDeaths, use.parallel=FALSE)
plot(forecast(fit))

taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit))
```

## License

This package is free and open source software, licensed under GPL (>= 2).
