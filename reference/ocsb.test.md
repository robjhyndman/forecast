# Osborn, Chui, Smith, and Birchenhall Test for Seasonal Unit Roots

An implementation of the Osborn, Chui, Smith, and Birchenhall (OCSB)
test.

## Usage

``` r
ocsb.test(x, lag.method = c("fixed", "AIC", "BIC", "AICc"), maxlag = 0)
```

## Arguments

- x:

  a univariate seasonal time series.

- lag.method:

  a character specifying the lag order selection method.

- maxlag:

  the maximum lag order to be considered by `lag.method`.

## Value

ocsb.test returns a list of class "OCSBtest" with the following
components:

- statistics the value of the test statistics.

- pvalues the p-values for each test statistics.

- method a character string describing the type of test.

- data.name a character string giving the name of the data.

- fitted.model the fitted regression model.

## Details

The regression equation may include lags of the dependent variable. When
lag.method = "fixed", the lag order is fixed to maxlag; otherwise,
maxlag is the maximum number of lags considered in a lag selection
procedure that minimises the lag.method criterion, which can be AIC or
BIC or corrected AIC, AICc, obtained as AIC + (2k(k+1))/(n-k-1), where k
is the number of parameters and n is the number of available
observations in the model.

Critical values for the test are based on simulations, which has been
smoothed over to produce critical values for all seasonal periods.

## References

Osborn DR, Chui APL, Smith J, and Birchenhall CR (1988) "Seasonality and
the order of integration for consumption", *Oxford Bulletin of Economics
and Statistics* **50**(4):361-377.

## See also

[`nsdiffs()`](https://pkg.robjhyndman.com/forecast/reference/nsdiffs.md)

## Examples

``` r
ocsb.test(AirPassengers)
#> 
#>  OCSB test
#> 
#> data:  AirPassengers
#> 
#> Test statistic: 1.5188, 5% critical value: -1.803
#> alternative hypothesis: stationary
#> 
#> Lag order 0 was selected using fixed
```
