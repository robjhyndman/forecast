# Box-Cox and Loess-based decomposition bootstrap.

Generates bootstrapped versions of a time series using the Box-Cox and
Loess-based decomposition bootstrap.

## Usage

``` r
bld.mbb.bootstrap(x, num, block_size = NULL)
```

## Arguments

- x:

  Original time series.

- num:

  Number of bootstrapped versions to generate.

- block_size:

  Block size for the moving block bootstrap.

## Value

A list with bootstrapped versions of the series. The first series in the
list is the original series.

## Details

The procedure is described in Bergmeir et al. Box-Cox decomposition is
applied, together with STL or Loess (for non-seasonal time series), and
the remainder is bootstrapped using a moving block bootstrap.

## References

Bergmeir, C., R. J. Hyndman, and J. M. Benitez (2016). Bagging
Exponential Smoothing Methods using STL Decomposition and Box-Cox
Transformation. International Journal of Forecasting 32, 303-312.

## See also

[`baggedETS()`](https://pkg.robjhyndman.com/forecast/reference/baggedModel.md).

## Author

Christoph Bergmeir, Fotios Petropoulos

## Examples

``` r
bootstrapped_series <- bld.mbb.bootstrap(WWWusage, 100)
```
