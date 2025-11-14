# Box Cox Transformation

BoxCox() returns a transformation of the input variable using a Box-Cox
transformation. InvBoxCox() reverses the transformation.

## Usage

``` r
BoxCox(x, lambda)

InvBoxCox(x, lambda, biasadj = FALSE, fvar = NULL)
```

## Arguments

- x:

  a numeric vector or time series of class `ts`.

- lambda:

  transformation parameter. If `lambda = "auto"`, then the
  transformation parameter lambda is chosen using BoxCox.lambda (with a
  lower bound of -0.9)

- biasadj:

  Use adjusted back-transformed mean for Box-Cox transformations. If
  transformed data is used to produce forecasts and fitted values, a
  regular back transformation will result in median forecasts. If
  biasadj is `TRUE`, an adjustment will be made to produce mean
  forecasts and fitted values.

- fvar:

  Optional parameter required if `biasadj = TRUE`. Can either be the
  forecast variance, or a list containing the interval `level`, and the
  corresponding `upper` and `lower` intervals.

## Value

a numeric vector of the same length as x.

## Details

The Box-Cox transformation (as given by Bickel & Doksum 1981) is given
by

\$\$f\_\lambda(x) =(sign(x)\|x\|^\lambda - 1)/\lambda\$\$

if \\\lambda\ne0\\. For \\\lambda=0\\,

\$\$f_0(x)=\log(x)\$\$.

## References

Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations.
*JRSS B* **26** 211–246. Bickel, P. J. and Doksum K. A. (1981) An
Analysis of Transformations Revisited. *JASA* **76** 296-311.

## See also

[`BoxCox.lambda()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.lambda.md)

## Author

Rob J Hyndman & Mitchell O'Hara-Wild

## Examples

``` r
lambda <- BoxCox.lambda(lynx)
lynx.fit <- ar(BoxCox(lynx, lambda))
plot(forecast(lynx.fit, h = 20, lambda = lambda))
#> Error in NextMethod(.Generic): cannot assign 'tsp' to zero-length vector
```
