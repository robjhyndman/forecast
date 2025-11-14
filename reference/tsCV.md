# Time series cross-validation

`tsCV` computes the forecast errors obtained by applying
`forecastfunction` to subsets of the time series `y` using a rolling
forecast origin.

## Usage

``` r
tsCV(y, forecastfunction, h = 1, window = NULL, xreg = NULL, initial = 0, ...)
```

## Arguments

- y:

  a numeric vector or univariate time series of class `ts`

- forecastfunction:

  Function to return an object of class `forecast`. Its first argument
  must be a univariate time series, and it must have an argument `h` for
  the forecast horizon. If exogenous predictors are used, then it must
  also have `xreg` and `newxreg` arguments corresponding to the training
  and test periods.

- h:

  Number of periods for forecasting. Default value is twice the largest
  seasonal period (for seasonal data) or ten (for non-seasonal data).

- window:

  Length of the rolling window, if NULL, a rolling window will not be
  used.

- xreg:

  Exogeneous predictor variables passed to the forecast function if
  required.

- initial:

  Initial period of the time series where no cross-validation is
  performed.

- ...:

  Other arguments are passed to `forecastfunction`.

## Value

Numerical time series object containing the forecast errors as a vector
(if h=1) and a matrix otherwise. The time index corresponds to the last
period of the training data. The columns correspond to the forecast
horizons.

## Details

Let `y` contain the time series \\y_1,\dots,y_T\\. Then
`forecastfunction` is applied successively to the time series
\\y_1,\dots,y_t\\, for \\t=1,\dots,T-h\\, making predictions
\\\hat{y}\_{t+h\|t}\\. The errors are given by \\e\_{t+h} =
y\_{t+h}-\hat{y}\_{t+h\|t}\\. If h=1, these are returned as a vector,
\\e_1,\dots,e_T\\. For h\>1, they are returned as a matrix with the hth
column containing errors for forecast horizon h. The first few errors
may be missing as it may not be possible to apply `forecastfunction` to
very short time series.

## See also

[`CV()`](https://pkg.robjhyndman.com/forecast/reference/CV.md),
[`CVar()`](https://pkg.robjhyndman.com/forecast/reference/CVar.md),
[`residuals.Arima()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md),
<https://robjhyndman.com/hyndsight/tscv/>.

## Author

Rob J Hyndman

## Examples

``` r
#Fit an AR(2) model to each rolling origin subset
far2 <- function(x, h) forecast(Arima(x, order = c(2, 0, 0)), h = h)
e <- tsCV(lynx, far2, h = 1)

#Fit the same model with a rolling window of length 30
e <- tsCV(lynx, far2, h = 1, window = 30)

#Example with exogenous predictors
far2_xreg <- function(x, h, xreg, newxreg) {
  forecast(Arima(x, order = c(2, 0, 0), xreg = xreg), xreg = newxreg)
}

y <- ts(rnorm(50))
xreg <- matrix(rnorm(100), ncol = 2)
e <- tsCV(y, far2_xreg, h = 3, xreg = xreg)
```
