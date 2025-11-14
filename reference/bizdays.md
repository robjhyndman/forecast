# Number of trading days in each season

Returns number of trading days in each month or quarter of the observed
time period in a major financial center.

## Usage

``` r
bizdays(x, FinCenter = c("New York", "London", "NERC", "Toronto", "Zurich"))
```

## Arguments

- x:

  Monthly or quarterly time series.

- FinCenter:

  Major financial center.

## Value

Time series

## Details

Useful for trading days length adjustments. More on how to define
"business days", please refer to
[`timeDate::isBizday()`](https://geobosh.github.io/timeDateDoc/reference/calendar-isBizday.html).

## See also

[`monthdays()`](https://pkg.robjhyndman.com/forecast/reference/monthdays.md)

## Author

Earo Wang

## Examples

``` r
x <- ts(rnorm(30), start = c(2013, 2), frequency = 12)
bizdays(x, FinCenter = "New York")
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 2013      19  20  22  22  20  22  22  20  23  20  21
#> 2014  21  19  21  21  21  21  22  21  21  23  19  22
#> 2015  20  19  22  21  20  22  22                    
```
