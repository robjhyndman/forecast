# Cross-validation statistic

Computes the leave-one-out cross-validation statistic (the mean of PRESS
– prediction residual sum of squares), AIC, corrected AIC, BIC and
adjusted R^2 values for a linear model.

## Usage

``` r
CV(obj)
```

## Arguments

- obj:

  Output from [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md).

## Value

Numerical vector containing CV, AIC, AICc, BIC and AdjR2 values.

## See also

[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html)

## Author

Rob J Hyndman

## Examples

``` r
y <- ts(rnorm(120, 0, 3) + 20 * sin(2 * pi * (1:120) / 12), frequency = 12)
fit1 <- tslm(y ~ trend + season)
fit2 <- tslm(y ~ season)
CV(fit1)
#>          CV         AIC        AICc         BIC       AdjR2 
#>  10.7275204 285.2056740 289.2056740 324.2305584   0.9537434 
CV(fit2)
#>          CV         AIC        AICc         BIC       AdjR2 
#>  10.6577638 284.6681111 288.1020734 320.9055038   0.9536098 
```
