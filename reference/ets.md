# Exponential smoothing state space model

Returns ets model applied to `y`.

## Usage

``` r
ets(
  y,
  model = "ZZZ",
  damped = NULL,
  alpha = NULL,
  beta = NULL,
  gamma = NULL,
  phi = NULL,
  additive.only = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  lower = c(rep(1e-04, 3), 0.8),
  upper = c(rep(0.9999, 3), 0.98),
  opt.crit = c("lik", "amse", "mse", "sigma", "mae"),
  nmse = 3,
  bounds = c("both", "usual", "admissible"),
  ic = c("aicc", "aic", "bic"),
  restrict = TRUE,
  allow.multiplicative.trend = FALSE,
  use.initial.values = FALSE,
  na.action = c("na.contiguous", "na.interp", "na.fail"),
  ...
)
```

## Arguments

- y:

  a numeric vector or univariate time series of class `ts`

- model:

  Usually a three-character string identifying method using the
  framework terminology of Hyndman et al. (2002) and Hyndman et al.
  (2008). The first letter denotes the error type ("A", "M" or "Z"); the
  second letter denotes the trend type ("N","A","M" or "Z"); and the
  third letter denotes the season type ("N","A","M" or "Z"). In all
  cases, "N"=none, "A"=additive, "M"=multiplicative and
  "Z"=automatically selected. So, for example, "ANN" is simple
  exponential smoothing with additive errors, "MAM" is multiplicative
  Holt-Winters' method with multiplicative errors, and so on.

  It is also possible for the model to be of class `ets`, and equal to
  the output from a previous call to `ets`. In this case, the same model
  is fitted to `y` without re-estimating any smoothing parameters. See
  also the `use.initial.values` argument.

- damped:

  If `TRUE`, use a damped trend (either additive or multiplicative). If
  `NULL`, both damped and non-damped trends will be tried and the best
  model (according to the information criterion `ic`) returned.

- alpha:

  Value of alpha. If `NULL`, it is estimated.

- beta:

  Value of beta. If `NULL`, it is estimated.

- gamma:

  Value of gamma. If `NULL`, it is estimated.

- phi:

  Value of phi. If `NULL`, it is estimated.

- additive.only:

  If `TRUE`, will only consider additive models. Default is `FALSE`.
  When `lambda` is specified, `additive.only` is set to `TRUE`.

- lambda:

  Box-Cox transformation parameter. If `lambda = "auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

- biasadj:

  Use adjusted back-transformed mean for Box-Cox transformations. If
  transformed data is used to produce forecasts and fitted values, a
  regular back transformation will result in median forecasts. If
  biasadj is `TRUE`, an adjustment will be made to produce mean
  forecasts and fitted values.

- lower:

  Lower bounds for the parameters (alpha, beta, gamma, phi). Ignored if
  `bounds = "admissible"`.

- upper:

  Upper bounds for the parameters (alpha, beta, gamma, phi). Ignored if
  `bounds = "admissible"`.

- opt.crit:

  Optimization criterion. One of "mse" (Mean Square Error), "amse"
  (Average MSE over first `nmse` forecast horizons), "sigma" (Standard
  deviation of residuals), "mae" (Mean of absolute residuals), or "lik"
  (Log-likelihood, the default).

- nmse:

  Number of steps for average multistep MSE (1\<=`nmse`\<=30).

- bounds:

  Type of parameter space to impose: `"usual"` indicates all parameters
  must lie between specified lower and upper bounds; `"admissible"`
  indicates parameters must lie in the admissible space; `"both"`
  (default) takes the intersection of these regions.

- ic:

  Information criterion to be used in model selection.

- restrict:

  If `TRUE` (default), the models with infinite variance will not be
  allowed.

- allow.multiplicative.trend:

  If `TRUE`, models with multiplicative trend are allowed when searching
  for a model. Otherwise, the model space excludes them. This argument
  is ignored if a multiplicative trend model is explicitly requested
  (e.g., using `model = "MMN"`).

- use.initial.values:

  If `TRUE` and `model` is of class `"ets"`, then the initial values in
  the model are also not re-estimated.

- na.action:

  A function which indicates what should happen when the data contains
  NA values. By default, the largest contiguous portion of the
  time-series will be used.

- ...:

  Other arguments are ignored.

## Value

An object of class `ets`.

The generic accessor functions `fitted.values` and `residuals` extract
useful features of the value returned by `ets` and associated functions.

## Details

Based on the classification of methods as described in Hyndman et al
(2008).

The methodology is fully automatic. The only required argument for ets
is the time series. The model is chosen automatically if not specified.
This methodology performed extremely well on the M3-competition data.
(See Hyndman, et al, 2002, below.)

## References

Hyndman, R.J., Koehler, A.B., Snyder, R.D., and Grose, S. (2002) "A
state space framework for automatic forecasting using exponential
smoothing methods", *International J. Forecasting*, **18**(3), 439–454.

Hyndman, R.J., Akram, Md., and Archibald, B. (2008) "The admissible
parameter space for exponential smoothing models". *Annals of
Statistical Mathematics*, **60**(2), 407–426.

Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D. (2008)
*Forecasting with exponential smoothing: the state space approach*,
Springer-Verlag. <http://www.exponentialsmoothing.net>.

## See also

[`stats::HoltWinters()`](https://rdrr.io/r/stats/HoltWinters.html),
[`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
[`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md).

## Author

Rob J Hyndman

## Examples

``` r
fit <- ets(USAccDeaths)
plot(forecast(fit))

```
