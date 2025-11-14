# Get response variable from time series model.

`getResponse` is a generic function for extracting the historical data
from a time series model (including `Arima`, `ets`, `ar`, `fracdiff`), a
linear model of class `lm`, or a forecast object. The function invokes
particular *methods* which depend on the class of the first argument.

## Usage

``` r
getResponse(object, ...)

# Default S3 method
getResponse(object, ...)

# S3 method for class 'lm'
getResponse(object, ...)

# S3 method for class 'Arima'
getResponse(object, ...)

# S3 method for class 'fracdiff'
getResponse(object, ...)

# S3 method for class 'ar'
getResponse(object, ...)

# S3 method for class 'tbats'
getResponse(object, ...)

# S3 method for class 'bats'
getResponse(object, ...)

# S3 method for class 'mforecast'
getResponse(object, ...)

# S3 method for class 'baggedModel'
getResponse(object, ...)
```

## Arguments

- object:

  a time series model or forecast object.

- ...:

  Additional arguments that are ignored.

## Value

A numerical vector or a time series object of class `ts`.

## Author

Rob J Hyndman
