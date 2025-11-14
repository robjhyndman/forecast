# Plot components from BATS model

Produces a plot of the level, slope and seasonal components from a BATS
or TBATS model. The plotted components are Box-Cox transformed using the
estimated transformation parameter.

## Usage

``` r
# S3 method for class 'bats'
plot(x, main = "Decomposition by BATS model", ...)

# S3 method for class 'tbats'
autoplot(object, range.bars = FALSE, ...)

# S3 method for class 'bats'
autoplot(object, range.bars = FALSE, ...)

# S3 method for class 'tbats'
plot(x, main = "Decomposition by TBATS model", ...)
```

## Arguments

- x:

  Object of class “bats/tbats”.

- main:

  Main title for plot.

- ...:

  Other plotting parameters passed to
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html).

- object:

  Object of class “bats/tbats”.

- range.bars:

  Logical indicating if each plot should have a bar at its right side
  representing relative size. If `NULL`, automatic selection takes
  place.

## Value

None. Function produces a plot

## See also

[`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md)\],
[`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- tbats(USAccDeaths)
plot(fit)
autoplot(fit, range.bars = TRUE)
} # }
```
