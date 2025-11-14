# Diebold-Mariano test for predictive accuracy

The Diebold-Mariano test compares the forecast accuracy of two forecast
methods.

## Usage

``` r
dm.test(
  e1,
  e2,
  alternative = c("two.sided", "less", "greater"),
  h = 1,
  power = 2,
  varestimator = c("acf", "bartlett")
)
```

## Arguments

- e1:

  Forecast errors from method 1.

- e2:

  Forecast errors from method 2.

- alternative:

  A character string specifying the alternative hypothesis, must be one
  of `"two.sided"` (default), `"greater"` or `"less"`. You can specify
  just the initial letter.

- h:

  The forecast horizon used in calculating `e1` and `e2`.

- power:

  The power used in the loss function. Usually 1 or 2.

- varestimator:

  A character string specifying the long-run variance estimator. Options
  are `"acf"` (default) or `"bartlett"`.

## Value

A list with class `htest` containing the following components:

- statistic:

  the value of the DM-statistic.

- parameter:

  the forecast horizon and loss function power used in the test.

- alternative:

  a character string describing the alternative hypothesis.

- varestimator:

  a character string describing the long-run variance estimator.

- p.value:

  the p-value for the test.

- method:

  a character string with the value "Diebold-Mariano Test".

- data.name:

  a character vector giving the names of the two error series.

## Details

This function implements the modified test proposed by Harvey, Leybourne
and Newbold (1997). The null hypothesis is that the two methods have the
same forecast accuracy. For `alternative = "less"`, the alternative
hypothesis is that method 2 is less accurate than method 1. For
`alternative = "greater"`, the alternative hypothesis is that method 2
is more accurate than method 1. For `alternative = "two.sided"`, the
alternative hypothesis is that method 1 and method 2 have different
levels of accuracy. The long-run variance estimator can either the
auto-correlation estimator `varestimator = "acf"`, or the estimator
based on Bartlett weights `varestimator = "bartlett"` which ensures a
positive estimate. Both long-run variance estimators are proposed in
Diebold and Mariano (1995).

## References

Diebold, F.X. and Mariano, R.S. (1995) Comparing predictive accuracy.
*Journal of Business and Economic Statistics*, **13**, 253-263.

Harvey, D., Leybourne, S., & Newbold, P. (1997). Testing the equality of
prediction mean squared errors. *International Journal of forecasting*,
**13**(2), 281-291.

## Author

George Athanasopoulos and Kirill Kuroptev

## Examples

``` r
# Test on in-sample one-step forecasts
f1 <- ets(WWWusage)
f2 <- auto.arima(WWWusage)
accuracy(f1)
#>                     ME    RMSE      MAE       MPE     MAPE      MASE      ACF1
#> Training set 0.2243266 3.40781 2.761668 0.2629465 2.162415 0.6102792 0.2308014
accuracy(f2)
#>                     ME     RMSE      MAE       MPE     MAPE      MASE
#> Training set 0.3035616 3.113754 2.405275 0.2805566 1.917463 0.5315228
#>                     ACF1
#> Training set -0.01715517
dm.test(residuals(f1), residuals(f2), h = 1)
#> 
#>  Diebold-Mariano Test
#> 
#> data:  residuals(f1)residuals(f2)
#> DM = 1.9078, Forecast horizon = 1, Loss function power = 2, p-value =
#> 0.05932
#> alternative hypothesis: two.sided
#> 

# Test on out-of-sample one-step forecasts
f1 <- ets(WWWusage[1:80])
f2 <- auto.arima(WWWusage[1:80])
f1.out <- ets(WWWusage[81:100], model = f1)
#> Model is being refit with current smoothing parameters but initial states are being re-estimated.
#> Set 'use.initial.values=TRUE' if you want to re-use existing initial values.
f2.out <- Arima(WWWusage[81:100], model = f2)
accuracy(f1.out)
#>                     ME    RMSE      MAE       MPE     MAPE      MASE      ACF1
#> Training set 0.2100836 3.24835 2.570459 0.1203497 1.352355 0.4246845 0.2287215
accuracy(f2.out)
#>                    ME     RMSE      MAE       MPE     MAPE      MASE
#> Training set 1.081679 3.329012 2.437119 0.6810673 1.375924 0.4026544
#>                      ACF1
#> Training set -0.004460367
dm.test(residuals(f1.out), residuals(f2.out), h = 1)
#> 
#>  Diebold-Mariano Test
#> 
#> data:  residuals(f1.out)residuals(f2.out)
#> DM = -0.14392, Forecast horizon = 1, Loss function power = 2, p-value =
#> 0.8871
#> alternative hypothesis: two.sided
#> 
```
