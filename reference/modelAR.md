# Time Series Forecasts with a user-defined model

Experimental function to forecast univariate time series with a
user-defined model

## Usage

``` r
modelAR(
  y,
  p,
  P = 1,
  FUN,
  predict.FUN,
  xreg = NULL,
  lambda = NULL,
  model = NULL,
  subset = NULL,
  scale.inputs = FALSE,
  x = y,
  ...
)
```

## Arguments

- y:

  a numeric vector or univariate time series of class `ts`

- p:

  Embedding dimension for non-seasonal time series. Number of
  non-seasonal lags used as inputs. For non-seasonal time series, the
  default is the optimal number of lags (according to the AIC) for a
  linear AR(p) model. For seasonal time series, the same method is used
  but applied to seasonally adjusted data (from an stl decomposition).

- P:

  Number of seasonal lags used as inputs.

- FUN:

  Function used for model fitting. Must accept argument `x` and `y` for
  the predictors and response, respectively (`formula` object not
  currently supported).

- predict.FUN:

  Prediction function used to apply `FUN` to new data. Must accept an
  object of class `FUN` as its first argument, and a data frame or
  matrix of new data for its second argument. Additionally, it should
  return fitted values when new data is omitted.

- xreg:

  Optionally, a numerical vector or matrix of external regressors, which
  must have the same number of rows as `y`. It should not be a data
  frame.

- lambda:

  Box-Cox transformation parameter. If `lambda = "auto"`, then a
  transformation is automatically selected using `BoxCox.lambda`. The
  transformation is ignored if NULL. Otherwise, data transformed before
  model is estimated.

- model:

  Output from a previous call to `nnetar`. If model is passed, this same
  model is fitted to `y` without re-estimating any parameters.

- subset:

  Optional vector specifying a subset of observations to be used in the
  fit. Can be an integer index vector or a logical vector the same
  length as `y`. All observations are used by default.

- scale.inputs:

  If `TRUE`, inputs are scaled by subtracting the column means and
  dividing by their respective standard deviations. If `lambda` is not
  `NULL`, scaling is applied after Box-Cox transformation.

- x:

  Deprecated. Included for backwards compatibility.

- ...:

  Other arguments passed to `FUN` for `modelAR`.

## Value

Returns an object of class `modelAR`.

The function `summary` is used to obtain and print a summary of the
results.

The generic accessor functions `fitted.values` and `residuals` extract
useful features of the value returned by `modelAR`.

- model:

  A list containing information about the fitted model

- method:

  The name of the forecasting method as a character string

- x:

  The original time series.

- xreg:

  The external regressors used in fitting (if given).

- residuals:

  Residuals from the fitted model. That is x minus fitted values.

- fitted:

  Fitted values (one-step forecasts)

- ...:

  Other arguments

## Details

This is an experimental function and only recommended for advanced
users. The selected model is fitted with lagged values of
`y as inputs. The inputs are for lags 1 to `p`, and lags `m`to`mP`where`m
=
frequency(y)`. If `xreg`is provided, its columns are also used as inputs. If there are missing values in`y`or`xreg\`,
the corresponding rows (and any others which depend on them as lags) are
omitted from the fit. The model is trained for one-step forecasting.
Multi-step forecasts are computed recursively.

## Author

Rob J Hyndman and Gabriel Caceres

## Examples

``` r
## Set up functions
my_lm <- function(x, y) {
 structure(lsfit(x,y), class = "lsfit")
}
predict.lsfit <- function(object, newdata = NULL) {
  n <- length(object$qr$qt)
  if(is.null(newdata)) {
    z <- numeric(n)
    z[seq_len(object$qr$rank)] <- object$qr$qt[seq_len(object$qr$rank)]
    as.numeric(qr.qy(object$qr, z))
  } else {
    sum(object$coefficients * c(1, newdata))
  }
}
# Fit an AR(2) model
fit <- modelAR(
  y = lynx,
  p = 2,
  FUN = my_lm,
  predict.FUN = predict.lsfit,
  lambda = 0.5,
  scale.inputs = TRUE
)
forecast(fit, h = 20) |> autoplot()
```
