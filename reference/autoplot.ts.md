# Automatically create a ggplot for time series objects

`autoplot` takes an object of type `ts` or `mts` and creates a ggplot
object suitable for usage with `stat_forecast`.

## Usage

``` r
# S3 method for class 'mts'
autolayer(object, colour = TRUE, series = NULL, ...)

# S3 method for class 'msts'
autolayer(object, series = NULL, ...)

# S3 method for class 'ts'
autolayer(object, colour = TRUE, series = NULL, ...)

# S3 method for class 'ts'
autoplot(
  object,
  series = NULL,
  xlab = "Time",
  ylab = deparse1(substitute(object)),
  main = NULL,
  ...
)

# S3 method for class 'mts'
autoplot(
  object,
  colour = TRUE,
  facets = FALSE,
  xlab = "Time",
  ylab = deparse1(substitute(object)),
  main = NULL,
  ...
)

# S3 method for class 'msts'
autoplot(object, ...)

# S3 method for class 'ts'
fortify(model, data, ...)
```

## Arguments

- object:

  Object of class `ts` or `mts`.

- colour:

  If `TRUE`, the time series will be assigned a colour aesthetic

- series:

  Identifies the time series with a colour, which integrates well with
  the functionality of
  [`geom_forecast()`](https://pkg.robjhyndman.com/forecast/reference/geom_forecast.md).

- ...:

  Other plotting parameters to affect the plot.

- xlab:

  X-axis label.

- ylab:

  Y-axis label.

- main:

  Main title.

- facets:

  If `TRUE`, multiple time series will be faceted (and unless specified,
  colour is set to `FALSE`). If `FALSE`, each series will be assigned a
  colour.

- model:

  Object of class `ts` to be converted to `data.frame`.

- data:

  Not used (required for
  [`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  method)

## Value

None. Function produces a ggplot graph.

## Details

`fortify.ts` takes a `ts` object and converts it into a data frame (for
usage with ggplot2).

## See also

[`stats::plot.ts()`](https://rdrr.io/r/stats/plot.ts.html),
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)

## Author

Mitchell O'Hara-Wild

## Examples

``` r
library(ggplot2)
autoplot(USAccDeaths)


lungDeaths <- cbind(mdeaths, fdeaths)
autoplot(lungDeaths)

autoplot(lungDeaths, facets = TRUE)

```
