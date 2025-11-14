# Easter holidays in each season

Returns a vector of 0's and 1's or fractional results if Easter spans
March and April in the observed time period. Easter is defined as the
days from Good Friday to Easter Sunday inclusively, plus optionally
Easter Monday if `easter.mon = TRUE`.

## Usage

``` r
easter(x, easter.mon = FALSE)
```

## Arguments

- x:

  Monthly or quarterly time series.

- easter.mon:

  If `TRUE`, the length of Easter holidays includes. Easter Monday.

## Value

Time series

## Details

Useful for adjusting calendar effects.

## Author

Earo Wang

## Examples

``` r
easter(wineind, easter.mon = TRUE)
#>       Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
#> 1980 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1981 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1982 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1983 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1984 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1985 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1986 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1987 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1988 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1989 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1990 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1991 0.00 0.00 0.75 0.25 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1992 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1993 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> 1994 0.00 0.00 0.00 1.00 0.00 0.00 0.00 0.00                    
```
