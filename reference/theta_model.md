# Theta model

The theta method of Assimakopoulos and Nikolopoulos (2000) is equivalent
to simple exponential smoothing with drift (Hyndman and Billah, 2003).
This function fits the theta model to a time series. The series is
tested for seasonality using the test outlined in A&N. If deemed
seasonal, the series is seasonally adjusted using a classical
multiplicative decomposition before fitting the theta model.

## Usage

``` r
theta_model(y, lambda = NULL, biasadj = FALSE)
```

## Arguments

- y:

  a numeric vector or univariate time series of class `ts`

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

An object of class `theta_model`.

## Details

More general theta methods are available in the
[forecTheta](https://CRAN.R-project.org/package=forecTheta) package.

## References

Assimakopoulos, V. and Nikolopoulos, K. (2000). The theta model: a
decomposition approach to forecasting. *International Journal of
Forecasting* **16**, 521-530.

Hyndman, R.J., and Billah, B. (2003) Unmasking the Theta method.
*International J. Forecasting*, **19**, 287-290.

## See also

[`thetaf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.md)

## Author

Rob J Hyndman

## Examples

``` r
nile_fit <- theta_model(Nile)
forecast(nile_fit) |> autoplot()
```
