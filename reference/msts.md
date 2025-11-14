# Multi-Seasonal Time Series

msts is an S3 class for multi seasonal time series objects, intended to
be used for models that support multiple seasonal periods. The msts
class inherits from the ts class and has an additional "msts" attribute
which contains the vector of seasonal periods. All methods that work on
a ts class, should also work on a msts class.

## Usage

``` r
msts(data, seasonal.periods, ts.frequency = floor(max(seasonal.periods)), ...)
```

## Arguments

- data:

  A numeric vector, ts object, matrix or data frame. It is intended that
  the time series data is univariate, otherwise treated the same as
  ts().

- seasonal.periods:

  A vector of the seasonal periods of the msts.

- ts.frequency:

  The seasonal period that should be used as frequency of the underlying
  ts object. The default value is `max(seasonal.periods)`.

- ...:

  Arguments to be passed to the underlying call to
  [`ts()`](https://rdrr.io/r/stats/ts.html). For example
  `start=c(1987, 5)`.

## Value

An object of class `c("msts", "ts")`. If there is only one seasonal
period (i.e., `length(seasonal.periods) == 1`), then the object is of
class `ts`.

## Author

Slava Razbash and Rob J Hyndman

## Examples

``` r
x <- msts(taylor, seasonal.periods = c(2 * 24, 2 * 24 * 7, 2 * 24 * 365), start = 2000 + 22 / 52)
y <- msts(USAccDeaths, seasonal.periods = 12, start = 1949)
```
