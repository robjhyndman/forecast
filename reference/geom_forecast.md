# Forecast plot

Generates forecasts from `forecast.ts` and adds them to the plot.
Forecasts can be modified via sending forecast specific arguments above.

## Usage

``` r
StatForecast

GeomForecast

geom_forecast(
  mapping = NULL,
  data = NULL,
  stat = "forecast",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  PI = TRUE,
  showgap = TRUE,
  series = NULL,
  ...
)
```

## Format

An object of class `StatForecast` (inherits from `Stat`, `ggproto`,
`gg`) of length 3.

An object of class `GeomForecast` (inherits from `Geom`, `ggproto`,
`gg`) of length 7.

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data.

- stat:

  The stat object to use calculate the data.

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function.

- na.rm:

  If `FALSE` (the default), removes missing values with a warning. If
  `TRUE` silently removes missing values.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`ggplot2::borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- PI:

  If `FALSE`, confidence intervals will not be plotted, giving only the
  forecast line.

- showgap:

  If `showgap = FALSE`, the gap between the historical observations and
  the forecasts is removed.

- series:

  Matches an unidentified forecast layer with a coloured object on the
  plot.

- ...:

  Additional arguments for
  [`forecast.ts()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md),
  other arguments are passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).
  These are often aesthetics, used to set an aesthetic to a fixed value,
  like `color = "red"` or `alpha = .5`. They may also be parameters to
  the paired geom/stat.

## Value

A layer for a ggplot graph.

## Details

Multivariate forecasting is supported by having each time series on a
different group.

You can also pass `geom_forecast` a `forecast` object to add it to the
plot.

The aesthetics required for the forecasting to work includes forecast
observations on the y axis, and the `time` of the observations on the x
axis. Refer to the examples below. To automatically set up aesthetics,
use `autoplot`.

## See also

[`generics::forecast()`](https://generics.r-lib.org/reference/forecast.html),
[`ggplot2::ggproto()`](https://ggplot2.tidyverse.org/reference/ggproto.html)

## Author

Mitchell O'Hara-Wild

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
autoplot(USAccDeaths) + geom_forecast()

lungDeaths <- cbind(mdeaths, fdeaths)
autoplot(lungDeaths) + geom_forecast()

# Using fortify.ts
p <- ggplot(aes(x = x, y = y), data = USAccDeaths)
p <- p + geom_line()
p + geom_forecast()

# Without fortify.ts
data <- data.frame(USAccDeaths = as.numeric(USAccDeaths),
                   time = as.numeric(time(USAccDeaths)))
p <- ggplot(aes(x = time, y = USAccDeaths), data = data)
p <- p + geom_line()
p + geom_forecast()

p + geom_forecast(h = 60)
p <- ggplot(aes(x = time, y = USAccDeaths), data = data)
p + geom_forecast(level = c(70, 98))
p + geom_forecast(level = c(70, 98), colour = "lightblue")

#Add forecasts to multivariate series with colour groups
lungDeaths <- cbind(mdeaths, fdeaths)
autoplot(lungDeaths) + geom_forecast(forecast(mdeaths), series = "mdeaths")
} # }
```
