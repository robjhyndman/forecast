# Fit a linear model with time series components

`tslm` is used to fit linear models to time series including trend and
seasonality components.

## Usage

``` r
tslm(formula, data, subset, lambda = NULL, biasadj = FALSE, ...)
```

## Arguments

- formula:

  An object of class "formula" (or one that can be coerced to that
  class): a symbolic description of the model to be fitted.

- data:

  An optional data frame, list or environment (or object coercible by
  as.data.frame to a data frame) containing the variables in the model.
  If not found in data, the variables are taken from
  environment(formula), typically the environment from which lm is
  called.

- subset:

  An optional subset containing rows of data to keep. For best results,
  pass a logical vector of rows to keep. Also supports
  [`subset()`](https://rdrr.io/r/base/subset.html) functions.

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

- ...:

  Other arguments passed to
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

## Value

Returns an object of class "lm".

## Details

`tslm` is largely a wrapper for
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) except that it allows
variables "trend" and "season" which are created on the fly from the
time series characteristics of the data. The variable "trend" is a
simple time trend and "season" is a factor indicating the season (e.g.,
the month or the quarter depending on the frequency of the data).

## See also

[`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md),
[`stats::lm()`](https://rdrr.io/r/stats/lm.html).

## Author

Mitchell O'Hara-Wild and Rob J Hyndman

## Examples

``` r
y <- ts(rnorm(120, 0, 3) + 1:120 + 20 * sin(2 * pi * (1:120) / 12), frequency = 12)
fit <- tslm(y ~ trend + season)
plot(forecast(fit, h = 20))

```
