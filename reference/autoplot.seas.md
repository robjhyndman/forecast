# Plot time series decomposition components using ggplot

Produces a ggplot object of seasonally decomposed time series for
objects of class `stl` (created with
[`stats::stl()`](https://rdrr.io/r/stats/stl.html), class `seas`
(created with
[`seasonal::seas()`](https://rdrr.io/pkg/seasonal/man/seas.html)), or
class `decomposed.ts` (created with
[`stats::decompose()`](https://rdrr.io/r/stats/decompose.html)).

## Usage

``` r
# S3 method for class 'decomposed.ts'
autoplot(object, labels = NULL, range.bars = NULL, ...)

# S3 method for class 'stl'
autoplot(object, labels = NULL, range.bars = TRUE, ...)

# S3 method for class 'StructTS'
autoplot(object, labels = NULL, range.bars = TRUE, ...)

# S3 method for class 'seas'
autoplot(object, labels = NULL, range.bars = NULL, ...)

# S3 method for class 'mstl'
autoplot(object, ...)
```

## Arguments

- object:

  Object of class `seas`, `stl`, or `decomposed.ts`.

- labels:

  Labels to replace "seasonal", "trend", and "remainder".

- range.bars:

  Logical indicating if each plot should have a bar at its right side
  representing relative size. If `NULL`, automatic selection takes
  place.

- ...:

  Other plotting parameters to affect the plot.

## Value

Returns an object of class `ggplot`.

## See also

[`seasonal::seas()`](https://rdrr.io/pkg/seasonal/man/seas.html),
[`stats::stl()`](https://rdrr.io/r/stats/stl.html),
[`stats::decompose()`](https://rdrr.io/r/stats/decompose.html),
[`stats::StructTS()`](https://rdrr.io/r/stats/StructTS.html),
[`stats::plot.stl()`](https://rdrr.io/r/stats/stlmethods.html).

## Author

Mitchell O'Hara-Wild

## Examples

``` r
library(ggplot2)
co2 |>
  decompose() |>
  autoplot()

nottem |>
  stl(s.window = "periodic") |>
  autoplot()

if (FALSE) { # \dontrun{
library(seasonal)
seas(USAccDeaths) |> autoplot()
} # }
```
