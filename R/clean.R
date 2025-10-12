# Functions to remove outliers and fill missing values in a time series
# Nothing for multiple seasonality yet.

# na.interp fills in missing values
# Uses linear interpolation for non-seasonal series
# Adds seasonality based on a periodic stl decomposition with seasonal series
# Argument lambda allows for Box-Cox transformation

#' Interpolate missing values in a time series
#'
#' By default, uses linear interpolation for non-seasonal series. For seasonal series, a
#' robust STL decomposition is first computed. Then a linear interpolation is applied to the
#' seasonally adjusted data, and the seasonal component is added back.
#'
#' A more general and flexible approach is available using `na.approx` in
#' the \CRANpkg{zoo} package.
#'
#' @param x Time series.
#' @param linear Should a linear interpolation be used.
#' @inheritParams forecast.ts
#' @return Time series
#' @author Rob J Hyndman
#' @seealso [tsoutliers()]
#' @keywords ts
#' @examples
#'
#' data(gold)
#' plot(na.interp(gold))
#'
#' @export
na.interp <- function(
  x,
  lambda = NULL,
  linear = (frequency(x) <= 1 || sum(!is.na(x)) <= 2 * frequency(x))
) {
  missng <- is.na(x)
  # Do nothing if no missing values
  if (sum(missng) == 0L) {
    return(x)
  }

  origx <- x
  rangex <- range(x, na.rm = TRUE)
  drangex <- rangex[2L] - rangex[1L]

  # Convert to ts
  if (is.null(tsp(x))) {
    x <- ts(x)
  }
  if (length(dim(x)) > 1) {
    if (NCOL(x) == 1) {
      x <- x[, 1]
    } else {
      stop("The time series is not univariate.")
    }
  }

  # Transform if requested
  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda = lambda)
    lambda <- attr(x, "lambda")
  }

  freq <- frequency(x)
  tspx <- tsp(x)
  n <- length(x)
  tt <- 1:n
  idx <- tt[!missng]

  if (linear) {
    # Use linear interpolation
    x <- ts(approx(idx, x[idx], tt, rule = 2)$y)
  } else {
    # Otherwise estimate seasonal component robustly
    # Then add to linear interpolation of seasonally adjusted series
    # Fit Fourier series for seasonality and a polynomial for the trend,
    # just to get something reasonable to start with
    if (inherits(x, "msts")) {
      K <- pmin(trunc(attributes(x)$msts / 2), 20L)
    } else {
      K <- min(trunc(freq / 2), 5)
    }
    X <- cbind(
      fourier(x, K),
      poly(tt, degree = pmin(pmax(trunc(n / 10), 1), 6L))
    )
    fit <- lm(x ~ X, na.action = na.exclude)
    pred <- predict(fit, newdata = data.frame(X))
    x[missng] <- pred[missng]
    # Now re-do it with stl to get better results
    fit <- mstl(x, robust = TRUE)
    # Interpolate seasonally adjusted values
    sa <- seasadj(fit)
    sa <- approx(idx, sa[idx], 1:n, rule = 2)$y
    # Replace original missing values
    seas <- seasonal(fit)
    if (NCOL(seas) > 1) {
      seas <- rowSums(seas)
    }
    x[missng] <- sa[missng] + seas[missng]
  }

  # Backtransform if required
  if (!is.null(lambda)) {
    x <- InvBoxCox(x, lambda = lambda)
  }

  # Ensure time series characteristics not lost
  tsp(x) <- tspx

  # Check stability and use linear interpolation if there is a problem
  if (
    !linear &&
      (max(x) > rangex[2L] + 0.5 * drangex ||
        min(x) < rangex[1L] - 0.5 * drangex)
  ) {
    return(na.interp(origx, lambda = lambda, linear = TRUE))
  } else {
    return(x)
  }
}

# Function to identify outliers and replace them with better values
# Missing values replaced as well if replace.missing=TRUE

#' Identify and replace outliers and missing values in a time series
#'
#' Uses supsmu for non-seasonal series and a robust STL decomposition for
#' seasonal series. To estimate missing values and outlier replacements,
#' linear interpolation is used on the (possibly seasonally adjusted) series
#'
#' @param x Time series.
#' @param replace.missing If `TRUE`, it not only replaces outliers, but
#' also interpolates missing values.
#' @param iterate The number of iterations required.
#' @inheritParams forecast.ts
#' @return Time series
#' @author Rob J Hyndman
#' @references Hyndman (2021) "Detecting time series outliers" \url{https://robjhyndman.com/hyndsight/tsoutliers/}.
#' @seealso [na.interp()], [tsoutliers()], [stats::supsmu()]
#' @keywords ts
#' @examples
#'
#' cleangold <- tsclean(gold)
#'
#' @export
tsclean <- function(x, replace.missing = TRUE, iterate = 2, lambda = NULL) {
  outliers <- tsoutliers(x, iterate = iterate, lambda = lambda)
  x[outliers$index] <- outliers$replacements
  if (replace.missing) {
    x <- na.interp(x, lambda = lambda)
  }
  x
}

# Function to identify time series outlieres

#' Identify and replace outliers in a time series
#'
#' Uses supsmu for non-seasonal series and a periodic stl decomposition with
#' seasonal series to identify outliers and estimate their replacements.
#'
#'
#' @param x Time series.
#' @param iterate The number of iterations required.
#' @inheritParams forecast.ts
#' @return \item{index}{Indicating the index of outlier(s)}
#' \item{replacement}{Suggested numeric values to replace identified outliers}
#' @author Rob J Hyndman
#' @seealso [na.interp()], [tsclean()]
#' @references Hyndman (2021) "Detecting time series outliers" \url{https://robjhyndman.com/hyndsight/tsoutliers/}.
#' @keywords ts
#' @examples
#'
#' data(gold)
#' tsoutliers(gold)
#'
#' @export
tsoutliers <- function(x, iterate = 2, lambda = NULL) {
  n <- length(x)
  freq <- frequency(x)

  # Identify and fill missing values
  missng <- is.na(x)
  nmiss <- sum(missng)
  if (nmiss > 0L) {
    xx <- na.interp(x, lambda = lambda)
  } else {
    xx <- x
  }

  # Check if constant
  if (is.constant(xx)) {
    return(list(index = integer(0), replacements = numeric(0)))
  }

  # Transform if requested
  if (!is.null(lambda)) {
    xx <- BoxCox(xx, lambda = lambda)
    lambda <- attr(xx, "lambda")
  }

  # Seasonally adjust data if necessary
  if (freq > 1 && n > 2 * freq) {
    fit <- mstl(xx, robust = TRUE)
    # Check if seasonality is sufficient to warrant adjustment
    rem <- remainder(fit)
    detrend <- xx - trendcycle(fit)
    strength <- 1 - var(rem) / var(detrend)
    if (strength >= 0.6) {
      xx <- seasadj(fit)
    }
  }

  # Use super-smoother on the (seasonally adjusted) data
  tt <- 1:n
  mod <- supsmu(tt, xx)
  resid <- xx - mod$y

  # Make sure missing values are not interpeted as outliers
  if (nmiss > 0L) {
    resid[missng] <- NA
  }

  # Limits of acceptable residuals
  resid.q <- quantile(resid, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + 3 * iqr * c(-1, 1)

  # Find residuals outside limits
  if ((limits[2] - limits[1]) > 1e-14) {
    outliers <- which((resid < limits[1]) | (resid > limits[2]))
  } else {
    outliers <- numeric(0)
  }

  # Replace all missing values including outliers
  x[outliers] <- NA
  x <- na.interp(x, lambda = lambda)

  # Do no more than 2 iterations regardless of the value of iterate
  if (iterate > 1) {
    tmp <- tsoutliers(x, iterate = 1, lambda = lambda)
    if (length(tmp$index) > 0) {
      # Found some more
      outliers <- sort(unique(c(outliers, tmp$index)))
      x[outliers] <- NA
      if (sum(!is.na(x)) == 1L) {
        # Only one non-missing value
        x[is.na(x)] <- x[!is.na(x)]
      } else {
        x <- na.interp(x, lambda = lambda)
      }
    }
  }

  # Return outlier indexes and replacements
  list(index = outliers, replacements = x[outliers])
}
