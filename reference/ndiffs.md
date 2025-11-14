# Number of differences required for a stationary series

Functions to estimate the number of differences required to make a given
time series stationary. `ndiffs` estimates the number of first
differences necessary.

## Usage

``` r
ndiffs(
  x,
  alpha = 0.05,
  test = c("kpss", "adf", "pp"),
  type = c("level", "trend"),
  max.d = 2,
  ...
)
```

## Arguments

- x:

  A univariate time series

- alpha:

  Level of the test, possible values range from 0.01 to 0.1.

- test:

  Type of unit root test to use

- type:

  Specification of the deterministic component in the regression

- max.d:

  Maximum number of non-seasonal differences allowed

- ...:

  Additional arguments to be passed on to the unit root test

## Value

An integer indicating the number of differences required for
stationarity.

## Details

`ndiffs` uses a unit root test to determine the number of differences
required for time series `x` to be made stationary. If `test = "kpss"`,
the KPSS test is used with the null hypothesis that `x` has a stationary
root against a unit-root alternative. Then the test returns the least
number of differences required to pass the test at the level `alpha`. If
`test = "adf"`, the Augmented Dickey-Fuller test is used and if
`test = "pp"` the Phillips-Perron test is used. In both of these cases,
the null hypothesis is that `x` has a unit root against a stationary
root alternative. Then the test returns the least number of differences
required to fail the test at the level `alpha`.

## References

Dickey DA and Fuller WA (1979), "Distribution of the Estimators for
Autoregressive Time Series with a Unit Root", *Journal of the American
Statistical Association* **74**:427-431.

Kwiatkowski D, Phillips PCB, Schmidt P and Shin Y (1992) "Testing the
Null Hypothesis of Stationarity against the Alternative of a Unit Root",
*Journal of Econometrics* **54**:159-178.

Osborn, D.R. (1990) "A survey of seasonality in UK macroeconomic
variables", *International Journal of Forecasting*, **6**:327-336.

Phillips, P.C.B. and Perron, P. (1988) "Testing for a unit root in time
series regression", *Biometrika*, **72**(2), 335-346.

Said E and Dickey DA (1984), "Testing for Unit Roots in Autoregressive
Moving Average Models of Unknown Order", *Biometrika* **71**:599-607.

## See also

[`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
and `ndiffs()`

## Author

Rob J Hyndman, Slava Razbash & Mitchell O'Hara-Wild

## Examples

``` r
ndiffs(WWWusage)
#> [1] 1
ndiffs(diff(log(AirPassengers), 12))
#> [1] 1
```
