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
#>  11.2586211 290.9296316 294.9296316 329.9545160   0.9504162 
CV(fit2)
#>          CV         AIC        AICc         BIC       AdjR2 
#>  11.1266227 289.8343573 293.2683196 326.0717500   0.9505035 
```
