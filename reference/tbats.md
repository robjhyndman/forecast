# TBATS model (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)

Fits a TBATS model applied to `y`, as described in De Livera, Hyndman &
Snyder (2011). Parallel processing is used by default to speed up the
computations.

## Usage

``` r
tbats(
  y,
  use.box.cox = NULL,
  use.trend = NULL,
  use.damped.trend = NULL,
  seasonal.periods = NULL,
  use.arma.errors = TRUE,
  use.parallel = length(y) > 1000,
  num.cores = 2,
  bc.lower = 0,
  bc.upper = 1,
  biasadj = FALSE,
  model = NULL,
  ...
)
```

## Arguments

- y:

  The time series to be forecast. Can be `numeric`, `msts` or `ts`. Only
  univariate time series are supported.

- use.box.cox:

  `TRUE`/`FALSE` indicates whether to use the Box-Cox transformation or
  not. If `NULL` then both are tried and the best fit is selected by
  AIC.

- use.trend:

  `TRUE`/`FALSE` indicates whether to include a trend or not. If `NULL`
  then both are tried and the best fit is selected by AIC.

- use.damped.trend:

  `TRUE`/`FALSE` indicates whether to include a damping parameter in the
  trend or not. If `NULL` then both are tried and the best fit is
  selected by AIC.

- seasonal.periods:

  If `y` is `numeric`, then seasonal periods can be specified with this
  parameter.

- use.arma.errors:

  `TRUE`/`FALSE` indicates whether to include ARMA errors or not. If
  `TRUE` the best fit is selected by AIC. If `FALSE` then the selection
  algorithm does not consider ARMA errors.

- use.parallel:

  `TRUE`/`FALSE` indicates whether or not to use parallel processing.

- num.cores:

  The number of parallel processes to be used if using parallel
  processing. If `NULL` then the number of logical cores is detected and
  all available cores are used.

- bc.lower:

  The lower limit (inclusive) for the Box-Cox transformation.

- bc.upper:

  The upper limit (inclusive) for the Box-Cox transformation.

- biasadj:

  Use adjusted back-transformed mean for Box-Cox transformations. If
  transformed data is used to produce forecasts and fitted values, a
  regular back transformation will result in median forecasts. If
  biasadj is `TRUE`, an adjustment will be made to produce mean
  forecasts and fitted values.

- model:

  Output from a previous call to `tbats`. If model is passed, this same
  model is fitted to `y` without re-estimating any parameters.

- ...:

  Additional arguments to be passed to `auto.arima` when choose an
  ARMA(p, q) model for the errors. (Note that xreg will be ignored, as
  will any arguments concerning seasonality and differencing, but
  arguments controlling the values of p and q will be used.)

## Value

An object with class `c("tbats", "bats")`. The generic accessor
functions
[`fitted.values()`](https://rdrr.io/r/stats/fitted.values.html) and
[`residuals()`](https://rdrr.io/r/stats/residuals.html) extract useful
features of the value returned by
[`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) and
associated functions. The fitted model is designated TBATS(omega, p,q,
phi, \<m1,k1\>,...,\<mJ,kJ\>) where omega is the Box-Cox parameter and
phi is the damping parameter; the error is modelled as an ARMA(p,q)
process and m1,...,mJ list the seasonal periods used in the model and
k1,...,kJ are the corresponding number of Fourier terms used for each
seasonality.

## References

De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011), Forecasting time
series with complex seasonal patterns using exponential smoothing,
*Journal of the American Statistical Association*, **106**(496),
1513-1527.

## See also

[`tbats.components()`](https://pkg.robjhyndman.com/forecast/reference/tbats.components.md).

## Author

Slava Razbash and Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- tbats(USAccDeaths)
plot(forecast(fit))

taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit))
} # }
```
