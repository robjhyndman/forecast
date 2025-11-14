# Number of differences required for a seasonally stationary series

Functions to estimate the number of differences required to make a given
time series stationary. `nsdiffs` estimates the number of seasonal
differences necessary.

## Usage

``` r
nsdiffs(
  x,
  alpha = 0.05,
  m = frequency(x),
  test = c("seas", "ocsb", "hegy", "ch"),
  max.D = 1,
  ...
)
```

## Arguments

- x:

  A univariate time series

- alpha:

  Level of the test, possible values range from 0.01 to 0.1.

- m:

  Deprecated. Length of seasonal period

- test:

  Type of unit root test to use

- max.D:

  Maximum number of seasonal differences allowed

- ...:

  Additional arguments to be passed on to the unit root test

## Value

An integer indicating the number of differences required for
stationarity.

## Details

`nsdiffs` uses seasonal unit root tests to determine the number of
seasonal differences required for time series `x` to be made stationary
(possibly with some lag-one differencing as well).

Several different tests are available:

- If `test = "seas"` (default), a measure of seasonal strength is used,
  where differencing is selected if the seasonal strength (Wang, Smith &
  Hyndman, 2006) exceeds 0.64 (based on minimizing MASE when forecasting
  using auto.arima on M3 and M4 data).

- If `test = "ch"`, the Canova-Hansen (1995) test is used (with null
  hypothesis of deterministic seasonality)

- If `test = "hegy"`, the Hylleberg, Engle, Granger & Yoo (1990) test is
  used.

- If `test = "ocsb"`, the Osborn-Chui-Smith-Birchenhall (1988) test is
  used (with null hypothesis that a seasonal unit root exists).

## References

Wang, X, Smith, KA, Hyndman, RJ (2006) "Characteristic-based clustering
for time series data", *Data Mining and Knowledge Discovery*, **13**(3),
335-364.

Osborn DR, Chui APL, Smith J, and Birchenhall CR (1988) "Seasonality and
the order of integration for consumption", *Oxford Bulletin of Economics
and Statistics* **50**(4):361-377.

Canova F and Hansen BE (1995) "Are Seasonal Patterns Constant over Time?
A Test for Seasonal Stability", *Journal of Business and Economic
Statistics* **13**(3):237-252.

Hylleberg S, Engle R, Granger C and Yoo B (1990) "Seasonal integration
and cointegration.", *Journal of Econometrics* **44**(1), pp. 215-238.

## See also

[`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
[`ndiffs()`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.md),
[`ocsb.test()`](https://pkg.robjhyndman.com/forecast/reference/ocsb.test.md),
[`uroot::hegy.test()`](https://geobosh.github.io/uroot/reference/hegy-test.html),
and
[`uroot::ch.test()`](https://geobosh.github.io/uroot/reference/ch-test.html)

## Author

Rob J Hyndman, Slava Razbash and Mitchell O'Hara-Wild

## Examples

``` r
nsdiffs(AirPassengers)
#> [1] 1
```
