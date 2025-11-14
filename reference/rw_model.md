# Random walk model

Fit a generalized random walk with Gaussian errors (and optional drift)
to a univariate time series.

## Usage

``` r
rw_model(y, lag = 1, drift = FALSE, lambda = NULL, biasadj = FALSE)
```

## Arguments

- y:

  a numeric vector or univariate time series of class `ts`

- lag:

  Lag parameter. `lag = 1` corresponds to a standard random walk (giving
  naive forecasts if `drift = FALSE` or drift forecasts if
  `drift = TRUE`), while `lag = m` corresponds to a seasonal random walk
  where m is the seasonal period (giving seasonal naive forecasts if
  `drift = FALSE`).

- drift:

  Logical flag. If `TRUE`, fits a random walk with drift model.

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

## Value

An object of class `rw_model`.

## Details

The model assumes that

\$\$Y_t = Y\_{t-p} + c + \varepsilon\_{t}\$\$

where \\p\\ is the lag parameter, \\c\\ is the drift parameter, and
\\\varepsilon_t\sim N(0,\sigma^2)\\ are iid.

The model without drift has \\c=0\\. In the model with drift, \\c\\ is
estimated by the sample mean of the differences \\Y_t - Y\_{t-p}\\.

If \\p=1\\, this is equivalent to an ARIMA(0,1,0) model with an optional
drift coefficient. For \\p\>1\\, it is equivalent to an
ARIMA(0,0,0)(0,1,0)p model.

The forecasts are given by

\$\$Y\_{T+h\|T}= Y\_{T+h-p(k+1)} + ch\$\$

where \\k\\ is the integer part of \\(h-1)/p\\. For a regular random
walk, \\p=1\\ and \\c=0\\, so all forecasts are equal to the last
observation. Forecast standard errors allow for uncertainty in
estimating the drift parameter (unlike the corresponding forecasts
obtained by fitting an ARIMA model directly).

The generic accessor functions
[`stats::fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
[`stats::residuals()`](https://rdrr.io/r/stats/residuals.html) extract
useful features of the object returned.

## See also

[`forecast.rw_model()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
[`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
[`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
[`snaive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)

## Examples

``` r
model <- rw_model(gold)
forecast(model, h = 50) |> autoplot()
```
