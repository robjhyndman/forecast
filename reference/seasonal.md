# Extract components from a time series decomposition

Returns a univariate time series equal to either a seasonal component,
trend-cycle component or remainder component from a time series
decomposition.

## Usage

``` r
seasonal(object)

trendcycle(object)

remainder(object)
```

## Arguments

- object:

  Object created by
  [`stats::decompose()`](https://rdrr.io/r/stats/decompose.html),
  [`stats::stl()`](https://rdrr.io/r/stats/stl.html) or
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md).

## Value

Univariate time series.

## See also

[`stats::stl()`](https://rdrr.io/r/stats/stl.html),
[`stats::decompose()`](https://rdrr.io/r/stats/decompose.html),
[`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md),
[`seasadj()`](https://pkg.robjhyndman.com/forecast/reference/seasadj.md).

## Author

Rob J Hyndman

## Examples

``` r
plot(USAccDeaths)
fit <- stl(USAccDeaths, s.window = "periodic")
lines(trendcycle(fit), col = "red")


library(ggplot2)
autoplot(
  cbind(
    Data = USAccDeaths,
    Seasonal = seasonal(fit),
    Trend = trendcycle(fit),
    Remainder = remainder(fit)
  ),
  facets = TRUE
) +
  labs(x = "Year", y = "")

```
