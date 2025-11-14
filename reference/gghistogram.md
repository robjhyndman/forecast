# Histogram with optional normal and kernel density functions

Plots a histogram and density estimates using ggplot.

## Usage

``` r
gghistogram(
  x,
  add.normal = FALSE,
  add.kde = FALSE,
  add.rug = TRUE,
  bins,
  boundary = 0
)
```

## Arguments

- x:

  a numerical vector.

- add.normal:

  Add a normal density function for comparison

- add.kde:

  Add a kernel density estimate for comparison

- add.rug:

  Add a rug plot on the horizontal axis

- bins:

  The number of bins to use for the histogram. Selected by default using
  the Friedman-Diaconis rule given by
  [`grDevices::nclass.FD()`](https://rdrr.io/r/grDevices/nclass.html)

- boundary:

  A boundary between two bins.

## Value

None.

## See also

[`graphics::hist()`](https://rdrr.io/r/graphics/hist.html),
[`ggplot2::geom_histogram()`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)

## Author

Rob J Hyndman

## Examples

``` r
gghistogram(lynx, add.kde = TRUE)

```
