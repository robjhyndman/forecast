### Functions to handle seasonality

#' Number of days in each season
#'
#' Returns number of days in each month or quarter of the observed time period.
#'
#' Useful for month length adjustments
#'
#' @param x time series
#' @return Time series
#' @author Rob J Hyndman
#' @seealso \code{\link[forecast]{bizdays}}
#' @keywords ts
#' @examples
#'
#' par(mfrow=c(2,1))
#' plot(ldeaths,xlab="Year",ylab="pounds",
#'     main="Monthly deaths from lung disease (UK)")
#' ldeaths.adj <- ldeaths/monthdays(ldeaths)*365.25/12
#' plot(ldeaths.adj,xlab="Year",ylab="pounds",
#'     main="Adjusted monthly deaths from lung disease (UK)")
#'
#' @export
monthdays <- function(x) {
  if (!is.ts(x)) {
    stop("Not a time series")
  }
  f <- frequency(x)
  if (f == 12) {
    days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  } else if (f == 4) {
    days <- c(90, 91, 92, 92)
  } else {
    stop("Not monthly or quarterly data")
  }
  nyears <- round(length(x) / f + 1) + 1
  years <- (1:nyears) + (start(x)[1] - 1)
  leap.years <- ((years %% 4 == 0) & !(years %% 100 == 0 & years %% 400 != 0))[1:nyears]
  dummy <- t(matrix(rep(days, nyears), nrow = f))
  if (f == 12) {
    dummy[leap.years, 2] <- 29
  } else {
    dummy[leap.years, 1] <- 91
  }
  xx <- c(t(dummy))[start(x)[2] - 1 + (1:length(x))]
  return(ts(xx, start = start(x), frequency = f))
}



#' Forecast seasonal index
#'
#' Returns vector containing the seasonal index for \code{h} future periods. If
#' the seasonal index is non-periodic, it uses the last values of the index.
#'
#'
#' @param object Output from \code{\link[stats]{decompose}} or
#' \link[stats]{stl}.
#' @param h Number of periods ahead to forecast
#' @return Time series
#' @author Rob J Hyndman
#' @keywords ts
#' @examples
#' uk.stl <- stl(UKDriverDeaths,"periodic")
#' uk.sa <- seasadj(uk.stl)
#' uk.fcast <- holt(uk.sa,36)
#' seasf <- sindexf(uk.stl,36)
#' uk.fcast$mean <- uk.fcast$mean + seasf
#' uk.fcast$lower <- uk.fcast$lower + cbind(seasf,seasf)
#' uk.fcast$upper <- uk.fcast$upper + cbind(seasf,seasf)
#' uk.fcast$x <- UKDriverDeaths
#' plot(uk.fcast,main="Forecasts from Holt's method with seasonal adjustment")
#'
#' @export
sindexf <- function(object, h) {
  if ("stl" %in% class(object)) {
    ss <- object$time.series[, 1]
    m <- frequency(ss)
    ss <- ss[length(ss) - (m:1) + 1]
    tsp.x <- tsp(object$time.series)
  }
  else if ("decomposed.ts" %in% class(object)) {
    ss <- object$figure
    m <- frequency(object$seasonal)
    n <- length(object$trend)
    ss <- rep(ss, n / m + 1)[1:n]
    ss <- ss[n - (m:1) + 1]
    tsp.x <- tsp(object$seasonal)
  }
  else {
    stop("Object of unknown class")
  }
  out <- ts(rep(ss, h / m + 1)[1:h], frequency = m, start = tsp.x[2] + 1 / m)

  return(out)
}




#' Seasonal dummy variables
#'
#' \code{seasonaldummy} returns a matrix of dummy variables suitable for use in
#' \code{\link{Arima}}, \code{\link{auto.arima}} or \code{\link{tslm}}. The
#' last season is omitted and used as the control.
#'
#' \code{seasonaldummyf} is deprecated, instead use the \code{h} argument in
#' \code{seasonaldummy}.
#'
#' The number of dummy variables is determined from the time series
#' characteristics of \code{x}. When \code{h} is missing, the length of
#' \code{x} also determines the number of rows for the matrix returned by
#' \code{seasonaldummy}. the value of \code{h} determines the number of rows
#' for the matrix returned by \code{seasonaldummy}, typically used for
#' forecasting. The values within \code{x} are not used.
#'
#' @param x Seasonal time series: a \code{ts} or a \code{msts} object
#' @param h Number of periods ahead to forecast (optional)
#' @return Numerical matrix.
#' @author Rob J Hyndman
#' @seealso \code{\link{fourier}}
#' @keywords ts
#' @examples
#'
#' plot(ldeaths)
#'
#' # Using seasonal dummy variables
#' month <- seasonaldummy(ldeaths)
#' deaths.lm  <- tslm(ldeaths ~ month)
#' tsdisplay(residuals(deaths.lm))
#' ldeaths.fcast <- forecast(deaths.lm,
#'     data.frame(month=I(seasonaldummy(ldeaths,36))))
#' plot(ldeaths.fcast)
#'
#' # A simpler approach to seasonal dummy variables
#' deaths.lm  <- tslm(ldeaths ~ season)
#' ldeaths.fcast <- forecast(deaths.lm, h=36)
#' plot(ldeaths.fcast)
#'
#' @export
seasonaldummy <- function(x, h=NULL) {
  if (!is.ts(x)) {
    stop("Not a time series")
  } else {
    fr.x <- frequency(x)
  }
  if (is.null(h)) {
    if (fr.x == 1) {
      stop("Non-seasonal time series")
    }
    dummy <- as.factor(cycle(x))
    dummy.mat <- matrix(0, ncol = frequency(x) - 1, nrow = length(x))
    nrow <- 1:length(x)
    for (i in 1:(frequency(x) - 1))
      dummy.mat[dummy == paste(i), i] <- 1
    colnames(dummy.mat) <- if (fr.x == 12) {
      month.abb[1:11]
    } else if (fr.x == 4) {
      c("Q1", "Q2", "Q3")
    } else {
      paste("S", 1:(fr.x - 1), sep = "")
    }

    return(dummy.mat)
  }
  else {
    return(seasonaldummy(ts(rep(0, h), start = tsp(x)[2] + 1 / fr.x, frequency = fr.x)))
  }
}

#' @rdname seasonaldummy
#' @export
seasonaldummyf <- function(x, h) {
  warning("seasonaldummyf() is deprecated, please use seasonaldummy()")
  if (!is.ts(x)) {
    stop("Not a time series")
  }
  f <- frequency(x)
  return(seasonaldummy(ts(rep(0, h), start = tsp(x)[2] + 1 / f, frequency = f)))
}



#' Fourier terms for modelling seasonality
#'
#' \code{fourier} returns a matrix containing terms from a Fourier series, up
#' to order \code{K}, suitable for use in \code{\link{Arima}},
#' \code{\link{auto.arima}}, or \code{\link{tslm}}.
#'
#' \code{fourierf} is deprecated, instead use the \code{h} argument in
#' \code{fourier}.
#'
#' The period of the Fourier terms is determined from the time series
#' characteristics of \code{x}. When \code{h} is missing, the length of
#' \code{x} also determines the number of rows for the matrix returned by
#' \code{fourier}. Otherwise, the value of \code{h} determines the number of
#' rows for the matrix returned by \code{fourier}, typically used for
#' forecasting. The values within \code{x} are not used.
#'
#' When \code{x} is a \code{ts} object, the value of \code{K} should be an
#' integer and specifies the number of sine and cosine terms to return. Thus,
#' the matrix returned has \code{2*K} columns.
#'
#' When \code{x} is a \code{msts} object, then \code{K} should be a vector of
#' integers specifying the number of sine and cosine terms for each of the
#' seasonal periods. Then the matrix returned will have \code{2*sum(K)}
#' columns.
#'
#' @param x Seasonal time series: a \code{ts} or a \code{msts} object
#' @param K Maximum order(s) of Fourier terms
#' @param h Number of periods ahead to forecast (optional)
#' @return Numerical matrix.
#' @author Rob J Hyndman
#' @seealso \code{\link{seasonaldummy}}
#' @keywords ts
#' @examples
#'
#' library(ggplot2)
#'
#' # Using Fourier series for a "ts" object
#' # K is chosen to minimize the AICc
#' deaths.model  <- auto.arima(USAccDeaths, xreg=fourier(USAccDeaths,K=5), seasonal=FALSE)
#' deaths.fcast <- forecast(deaths.model, xreg=fourier(USAccDeaths, K=5, h=36))
#' autoplot(deaths.fcast) + xlab("Year")
#'
#' # Using Fourier series for a "msts" object
#' taylor.lm <- tslm(taylor ~ fourier(taylor, K = c(3, 3)))
#' taylor.fcast <- forecast(taylor.lm,
#'     data.frame(fourier(taylor, K = c(3, 3), h = 270)))
#' autoplot(taylor.fcast)
#'
#' @export
fourier <- function(x, K, h=NULL) {
  if (is.null(h)) {
    return(...fourier(x, K, 1:NROW(x)))
  }
  else {
    return(...fourier(x, K, NROW(x) + (1:h)))
  }
}

#' @rdname fourier
#' @export
fourierf <- function(x, K, h) {
  warning("fourierf() is deprecated, please use fourier()")
  return(...fourier(x, K, length(x) + (1:h)))
}


# Function to do the work.
...fourier <- function(x, K, times) {
  if (any(class(x) == "msts")) {
    period <- attr(x, "msts")
  } else {
    period <- frequency(x)
  }

  # Patch for older versions of R that do not have sinpi and cospi functions.
  if (!exists("sinpi")) {
    sinpi <- function(x) {
      sin(pi * x)
    }
    cospi <- function(x) {
      cos(pi * x)
    }
  }

  if (length(period) != length(K)) {
    stop("Number of periods does not match number of orders")
  }
  if (any(2 * K > period)) {
    stop("K must be not be greater than period/2")
  }

  # Compute periods of all Fourier terms
  p <- numeric(0)
  labels <- character(0)
  for (j in seq_along(period))
  {
    if (K[j] > 0) {
      p <- c(p, (1:K[j]) / period[j])
      labels <- c(labels, paste(
        paste0(c("S", "C"), rep(1:K[j], rep(2, K[j]))),
        round(period[j]), sep = "-"
      ))
    }
  }
  # Remove equivalent seasonal periods due to multiple seasonality
  k <- duplicated(p)
  p <- p[!k]
  labels <- labels[!rep(k, rep(2, length(k)))]

  # Remove columns where sinpi=0
  k <- abs(2 * p - round(2 * p)) > .Machine$double.eps

  # Compute matrix of Fourier terms
  X <- matrix(NA_real_, nrow = length(times), ncol = 2L * length(p))
  for (j in seq_along(p))
  {
    if (k[j]) {
      X[, 2L * j - 1L] <- sinpi(2 * p[j] * times)
    }
    X[, 2L * j] <- cospi(2 * p[j] * times)
  }
  colnames(X) <- labels

  # Remove missing columns
  X <- X[, !is.na(colSums(X)), drop = FALSE]

  return(X)
}



#' Moving-average smoothing
#'
#' \code{ma} computes a simple moving average smoother of a given time series.
#'
#' The moving average smoother averages the nearest \code{order} periods of
#' each observation. As neighbouring observations of a time series are likely
#' to be similar in value, averaging eliminates some of the randomness in the
#' data, leaving a smooth trend-cycle component. \deqn{\hat{T}_{t} =
#' \frac{1}{m} \sum_{j=-k}^k
#' y_{t+j}}{T[t]=1/m(y[t-k]+y[t-k+1]+\ldots+y[t]+\ldots+y[t+k-1]+y[t+k])} where
#' \eqn{k=\frac{m-1}{2}}{k=(m-1)/2}
#'
#' When an even \code{order} is specified, the observations averaged will
#' include one more observation from the future than the past (k is rounded
#' up). If centre is TRUE, the value from two moving averages (where k is
#' rounded up and down respectively) are averaged, centering the moving
#' average.
#'
#' @param x Univariate time series
#' @param order Order of moving average smoother
#' @param centre If TRUE, then the moving average is centred for even orders.
#' @return Numerical time series object containing the simple moving average
#' smoothed values.
#' @author Rob J Hyndman
#' @seealso \code{\link[stats]{decompose}}
#' @keywords ts
#' @examples
#'
#' plot(wineind)
#' sm <- ma(wineind,order=12)
#' lines(sm,col="red")
#'
#' @export
ma <- function(x, order, centre=TRUE) {
  if (abs(order - round(order)) > 1e-8) {
    stop("order must be an integer")
  }

  if (order %% 2 == 0 && centre) { # centred and even
    w <- c(0.5, rep(1, order - 1), 0.5) / order
  } else { # odd or not centred
    w <- rep(1, order) / order
  }

  return(filter(x, w))
}
