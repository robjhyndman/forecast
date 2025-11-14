# Number of days in each season

Returns number of days in each month or quarter of the observed time
period.

## Usage

``` r
monthdays(x)
```

## Arguments

- x:

  time series

## Value

Time series

## Details

Useful for month length adjustments

## See also

[`bizdays()`](https://pkg.robjhyndman.com/forecast/reference/bizdays.md)

## Author

Rob J Hyndman

## Examples

``` r
par(mfrow = c(2, 1))
plot(
  ldeaths,
  xlab = "Year",
  ylab = "pounds",
  main = "Monthly deaths from lung disease (UK)"
)
ldeaths.adj <- ldeaths / monthdays(ldeaths) * 365.25 / 12
plot(
  ldeaths.adj,
  xlab = "Year",
  ylab = "pounds",
  main = "Adjusted monthly deaths from lung disease (UK)"
)

```
