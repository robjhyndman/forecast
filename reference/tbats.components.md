# Extract components of a TBATS model

Extract the level, slope and seasonal components of a TBATS model. The
extracted components are Box-Cox transformed using the estimated
transformation parameter.

## Usage

``` r
tbats.components(x)
```

## Arguments

- x:

  A tbats object created by
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md).

## Value

A multiple time series (`mts`) object. The first series is the observed
time series. The second series is the trend component of the fitted
model. Series three onwards are the seasonal components of the fitted
model with one time series for each of the seasonal components. All
components are transformed using estimated Box-Cox parameter.

## References

De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011), Forecasting time
series with complex seasonal patterns using exponential smoothing,
*Journal of the American Statistical Association*, **106**(496),
1513-1527.

## See also

[`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md).

## Author

Slava Razbash and Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- tbats(USAccDeaths, use.parallel = FALSE)
components <- tbats.components(fit)
plot(components)
} # }
```
