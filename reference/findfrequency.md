# Find dominant frequency of a time series

`findfrequency` returns the period of the dominant frequency of a time
series. For seasonal data, it will return the seasonal period. For
cyclic data, it will return the average cycle length.

## Usage

``` r
findfrequency(x)
```

## Arguments

- x:

  a numeric vector or time series of class `ts`

## Value

an integer value

## Details

The dominant frequency is determined from a spectral analysis of the
time series. First, a linear trend is removed, then the spectral density
function is estimated from the best fitting autoregressive model (based
on the AIC). If there is a large (possibly local) maximum in the
spectral density function at frequency \\f\\, then the function will
return the period \\1/f\\ (rounded to the nearest integer). If no such
dominant frequency can be found, the function will return 1.

## Author

Rob J Hyndman

## Examples

``` r
findfrequency(USAccDeaths) # Monthly data
#> [1] 12
findfrequency(taylor) # Half-hourly data
#> [1] 48
findfrequency(lynx) # Annual data
#> [1] 10
```
