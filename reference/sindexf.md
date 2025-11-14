# Forecast seasonal index

Returns vector containing the seasonal index for `h` future periods. If
the seasonal index is non-periodic, it uses the last values of the
index.

## Usage

``` r
sindexf(object, h)
```

## Arguments

- object:

  Output from
  [`stats::decompose()`](https://rdrr.io/r/stats/decompose.html) or
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html).

- h:

  Number of periods ahead to forecast.

## Value

Time series

## Author

Rob J Hyndman

## Examples

``` r
uk.stl <- stl(UKDriverDeaths, "periodic")
uk.sa <- seasadj(uk.stl)
uk.fcast <- holt(uk.sa, 36)
seasf <- sindexf(uk.stl, 36)
uk.fcast$mean <- uk.fcast$mean + seasf
uk.fcast$lower <- uk.fcast$lower + cbind(seasf, seasf)
uk.fcast$upper <- uk.fcast$upper + cbind(seasf, seasf)
uk.fcast$x <- UKDriverDeaths
plot(uk.fcast, main = "Forecasts from Holt's method with seasonal adjustment")

```
