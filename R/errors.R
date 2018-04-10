## Measures of forecast accuracy
## Forecasts in f. This may be a numerical vector or the output from arima or ets or derivatives.
## Actual values in x
# dx = response variable in historical data
## test enables a subset of x and f to be tested.
# MASE: d is the # of differencing
# MASE: D is the # of seasonal differencing
testaccuracy <- function(f, x, test, d, D) {
  dx <- getResponse(f)
  if (is.data.frame(x)) {
    responsevar <- as.character(formula(f$model))[2]
    if (is.element(responsevar, colnames(x))) {
      x <- x[, responsevar]
    } else {
      stop("I can't figure out what data to use.")
    }
  }
  if (is.list(f)) {
    if (is.element("mean", names(f))) {
      f <- f$mean
    } else {
      stop("Unknown list structure")
    }
  }
  if (is.ts(x) && is.ts(f)) {
    tspf <- tsp(f)
    tspx <- tsp(x)
    start <- max(tspf[1], tspx[1])
    end <- min(tspf[2], tspx[2])
    # Adjustment to allow for floating point issues
    start <- min(start, end)
    end <- max(start, end)
    f <- window(f, start = start, end = end)
    x <- window(x, start = start, end = end)
  }
  n <- length(x)
  if (is.null(test)) {
    test <- 1:n
  } else if (min(test) < 1 || max(test) > n) {
    warning("test elements must be within sample")
    test <- test[test >= 1 & test <= n]
  }

  ff <- f
  xx <- x

  # Check length of f
  if (length(f) < n) {
    stop("Not enough forecasts. Check that forecasts and test data match.")
  }

  error <- (xx - ff[1:n])[test]
  pe <- error / xx[test] * 100

  me <- mean(error, na.rm = TRUE)
  mse <- mean(error ^ 2, na.rm = TRUE)
  mae <- mean(abs(error), na.rm = TRUE)
  mape <- mean(abs(pe), na.rm = TRUE)
  mpe <- mean(pe, na.rm = TRUE)
  out <- c(me, sqrt(mse), mae, mpe, mape)
  names(out) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")

  # Compute MASE if historical data available
  if (!is.null(dx)) {
    tspdx <- tsp(dx)
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale <- mean(abs(nd), na.rm = TRUE)
    } else { # not time series
      scale <- mean(abs(dx - mean(dx, na.rm = TRUE)), na.rm = TRUE)
    }
    mase <- mean(abs(error / scale), na.rm = TRUE)
    out <- c(out, mase)
    names(out)[length(out)] <- "MASE"
  }

  # Additional time series measures
  if (!is.null(tsp(x)) && n > 1) {
    fpe <- (c(ff[2:n]) / c(xx[1:(n - 1)]) - 1)[test - 1]
    ape <- (c(xx[2:n]) / c(xx[1:(n - 1)]) - 1)[test - 1]
    theil <- sqrt(sum((fpe - ape) ^ 2, na.rm = TRUE) / sum(ape ^ 2, na.rm = TRUE))
    if (length(error) > 1) {
      r1 <- acf(error, plot = FALSE, lag.max = 2, na.action = na.pass)$acf[2, 1, 1]
    } else {
      r1 <- NA
    }
    nj <- length(out)
    out <- c(out, r1, theil)
    names(out)[nj + (1:2)] <- c("ACF1", "Theil's U")
  }

  return(out)
}


trainingaccuracy <- function(f, test, d, D) {
  # Make sure x is an element of f when f is a fitted model rather than a forecast
  # if(!is.list(f))
  #  stop("f must be a forecast object or a time series model object.")
  dx <- getResponse(f)
  if (is.element("splineforecast", class(f))) {
    fits <- f$onestepf
  } else {
    fits <- fitted(f)
  } # Don't use f$resid as this may contain multiplicative errors.

  res <- dx - fits
  n <- length(res)
  if (is.null(test)) {
    test <- 1:n
  }
  if (min(test) < 1 || max(test) > n) {
    warning("test elements must be within sample")
    test <- test[test >= 1 & test <= n]
  }

  tspdx <- tsp(dx)

  res <- res[test]
  dx <- dx[test]
  pe <- res / dx * 100 # Percentage error

  me <- mean(res, na.rm = TRUE)
  mse <- mean(res ^ 2, na.rm = TRUE)
  mae <- mean(abs(res), na.rm = TRUE)
  mape <- mean(abs(pe), na.rm = TRUE)
  mpe <- mean(pe, na.rm = TRUE)
  out <- c(me, sqrt(mse), mae, mpe, mape)
  names(out) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")

  # Compute MASE if historical data available
  if (!is.null(dx)) {
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale <- mean(abs(nd), na.rm = TRUE)
    } else { # not time series
      scale <- mean(abs(dx - mean(dx, na.rm = TRUE)), na.rm = TRUE)
    }
    mase <- mean(abs(res / scale), na.rm = TRUE)
    out <- c(out, mase)
    names(out)[length(out)] <- "MASE"
  }

  # Additional time series measures
  if (!is.null(tspdx)) {
    if (length(res) > 1) {
      r1 <- acf(res, plot = FALSE, lag.max = 2, na.action = na.pass)$acf[2, 1, 1]
    } else {
      r1 <- NA
    }
    nj <- length(out)
    out <- c(out, r1)
    names(out)[nj + 1] <- "ACF1"
  }

  return(out)
}



#' Accuracy measures for a forecast model
#'
#' Returns range of summary measures of the forecast accuracy. If \code{x} is
#' provided, the function measures test set forecast accuracy
#' based on \code{x-f}. If \code{x} is not provided, the function only produces
#' training set accuracy measures of the forecasts based on
#' \code{f["x"]-fitted(f)}. All measures are defined and discussed in Hyndman
#' and Koehler (2006).
#'
#' The measures calculated are:
#' \itemize{
#'   \item ME: Mean Error
#'   \item RMSE: Root Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MPE: Mean Percentage Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item MASE: Mean Absolute Scaled Error
#'   \item ACF1: Autocorrelation of errors at lag 1.
#' }
#' By default, the MASE calculation is scaled using MAE of training set naive
#' forecasts for non-seasonal time series, training set seasonal naive forecasts
#' for seasonal time series and training set mean forecasts for non-time series data.
#' If \code{f} is a numerical vector rather than a \code{forecast} object, the MASE
#' will not be returned as the training data will not be available.
#'
#' See Hyndman and Koehler (2006) and Hyndman and Athanasopoulos (2014, Section
#' 2.5) for further details.
#'
#' @param f An object of class \dQuote{\code{forecast}}, or a numerical vector
#' containing forecasts. It will also work with \code{Arima}, \code{ets} and
#' \code{lm} objects if \code{x} is omitted -- in which case training set accuracy
#' measures are returned.
#' @param x An optional numerical vector containing actual values of the same
#' length as object, or a time series overlapping with the times of \code{f}.
#' @param test Indicator of which elements of \code{x} and \code{f} to test. If
#' \code{test} is \code{NULL}, all elements are used. Otherwise test is a
#' numeric vector containing the indices of the elements to use in the test.
#' @param d An integer indicating the number of lag-1 differences to be used
#' for the denominator in MASE calculation. Default value is 1 for non-seasonal
#' series and 0 for seasonal series.
#' @param D An integer indicating the number of seasonal differences to be used
#' for the denominator in MASE calculation. Default value is 0 for non-seasonal
#' series and 1 for seasonal series.
#' @param ... Additional arguments depending on the specific method.
#' @return Matrix giving forecast accuracy measures.
#' @author Rob J Hyndman
#' @references Hyndman, R.J. and Koehler, A.B. (2006) "Another look at measures
#' of forecast accuracy". \emph{International Journal of Forecasting},
#' \bold{22}(4), 679-688. Hyndman, R.J. and Athanasopoulos, G. (2018)
#' "Forecasting: principles and practice", 2nd ed., OTexts, Melbourne, Australia. 
#' Section 3.4 "Evaluating forecast accuracy". 
#' \url{https://otexts.org/fpp2/accuracy.html}.
#' @keywords ts
#' @examples
#'
#' fit1 <- rwf(EuStockMarkets[1:200,1],h=100)
#' fit2 <- meanf(EuStockMarkets[1:200,1],h=100)
#' accuracy(fit1)
#' accuracy(fit2)
#' accuracy(fit1,EuStockMarkets[201:300,1])
#' accuracy(fit2,EuStockMarkets[201:300,1])
#' plot(fit1)
#' lines(EuStockMarkets[1:300,1])
#' @export
accuracy <- function(f, ...) {
  UseMethod("accuracy")
}

#' @rdname accuracy
#' @method accuracy default
#' @export
accuracy.default <- function(f, x, test=NULL, d=NULL, D=NULL, ...) {
  if (!any(is.element(class(f), c(
    "mforecast", "forecast", "ts", "integer", "numeric",
    "Arima", "ets", "lm", "bats", "tbats", "nnetar", "stlm", "baggedModel"
  )))) {
    stop("First argument should be a forecast object or a time series.")
  }
  if (is.element("mforecast", class(f))) {
    return(accuracy.mforecast(f, x, test, d, D))
  }

  trainset <- (is.list(f))
  testset <- (!missing(x))
  if (testset && !is.null(test)) {
    trainset <- FALSE
  }
  if (!trainset && !testset) {
    stop("Unable to compute forecast accuracy measures")
  }

  # Find d and D
  if (is.null(D) && is.null(d)) {
    if (testset) {
      d <- as.numeric(frequency(x) == 1)
      D <- as.numeric(frequency(x) > 1)
    }
    else if (trainset) {
      if (!is.null(f$mean)) {
        d <- as.numeric(frequency(f$mean) == 1)
        D <- as.numeric(frequency(f$mean) > 1)
      }
      else {
        d <- as.numeric(frequency(f$x) == 1)
        D <- as.numeric(frequency(f$x) > 1)
      }
    }
    else {
      d <- as.numeric(frequency(f) == 1)
      D <- as.numeric(frequency(f) > 1)
    }
  }


  if (trainset) {
    trainout <- trainingaccuracy(f, test, d, D)
    trainnames <- names(trainout)
  }
  else {
    trainnames <- NULL
  }
  if (testset) {
    testout <- testaccuracy(f, x, test, d, D)
    testnames <- names(testout)
  }
  else {
    testnames <- NULL
  }
  outnames <- unique(c(trainnames, testnames))

  out <- matrix(NA, nrow = 2, ncol = length(outnames))
  colnames(out) <- outnames
  rownames(out) <- c("Training set", "Test set")
  if (trainset) {
    out[1, names(trainout)] <- trainout
  }
  if (testset) {
    out[2, names(testout)] <- testout
  }

  if (!testset) {
    out <- out[1, , drop = FALSE]
  }
  if (!trainset) {
    out <- out[2, , drop = FALSE]
  }
  return(out)
}

# Compute accuracy for an mforecast object
#' @export
accuracy.mforecast <- function(f, x, test=NULL, d, D, ...) {
  object <- f
  out <- NULL
  nox <- missing(x)
  i <- 1
  for (fcast in object$forecast)
  {
    if (nox) {
      out1 <- accuracy(fcast, test = test, d = d, D = D)
    } else {
      out1 <- accuracy(fcast, x[, i], test, d, D)
    }
    rownames(out1) <- paste(fcast$series, rownames(out1))
    out <- rbind(out, out1)
    i <- i + 1
  }
  return(out)
}
