#' Forecasting using BATS and TBATS models
#'
#' Forecasts `h` steps ahead with a BATS model. Prediction intervals are
#' also produced.
#'
#' @inheritParams forecast.ets
#' @param object An object of class `bats`. Usually the result of a call to
#' [bats()].
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Slava Razbash and Rob J Hyndman
#' @seealso [bats()], [tbats()], [forecast.ets()].
#' @references De Livera, A.M., Hyndman, R.J., & Snyder, R. D. (2011),
#' Forecasting time series with complex seasonal patterns using exponential
#' smoothing, \emph{Journal of the American Statistical Association},
#' \bold{106}(496), 1513-1527.
#' @keywords ts
#' @examples
#'
#' \dontrun{
#' fit <- bats(USAccDeaths)
#' plot(forecast(fit))
#'
#' taylor.fit <- bats(taylor)
#' plot(forecast(taylor.fit))
#' }
#'
#' @export
forecast.bats <- function(
  object,
  h,
  level = c(80, 95),
  fan = FALSE,
  biasadj = NULL,
  ...
) {
  # Set up the variables
  if (is.ts(object$y)) {
    ts.frequency <- frequency(object$y)
  } else if (!is.null(object$seasonal.periods)) {
    ts.frequency <- max(object$seasonal.periods)
  } else {
    ts.frequency <- 1
  }
  if(is.null(biasadj)) {
    if(!is.null(object$lambda)) {
      biasadj <- attr(object$lambda, "biasadj")
    } else {
      biasadj <- FALSE
    }
  }

  if (missing(h)) {
    if (is.null(object$seasonal.periods)) {
      h <- if (ts.frequency == 1) 10 else 2 * ts.frequency
    } else {
      h <- 2 * max(object$seasonal.periods)
    }
  } else if (h <= 0) {
    stop("Forecast horizon out of bounds")
  }

  level <- getConfLevel(level, fan)

  # Set up the matrices
  x <- matrix(0, nrow = nrow(object$x), ncol = h)
  y.forecast <- numeric(h)
  # w <- makeWMatrix(small.phi=object$damping.parameter, seasonal.periods=object$seasonal.periods, ar.coefs=object$ar.coefficients, ma.coefs=object$ma.coefficients)
  w <- .Call(
    "makeBATSWMatrix",
    smallPhi_s = object$damping.parameter,
    sPeriods_s = object$seasonal.periods,
    arCoefs_s = object$ar.coefficients,
    maCoefs_s = object$ma.coefficients,
    PACKAGE = "forecast"
  )
  # g <- makeGMatrix(alpha=object$alpha, beta=object$beta, gamma.vector=object$gamma.values, seasonal.periods=object$seasonal.periods, p=length(object$ar.coefficients), q=length(object$ma.coefficients))
  g <- .Call(
    "makeBATSGMatrix",
    object$alpha,
    object$beta,
    object$gamma.values,
    object$seasonal.periods,
    length(object$ar.coefficients),
    length(object$ma.coefficients),
    PACKAGE = "forecast"
  )

  F <- makeFMatrix(
    alpha = object$alpha,
    beta = object$beta,
    small.phi = object$damping.parameter,
    seasonal.periods = object$seasonal.periods,
    gamma.bold.matrix = g$gamma.bold.matrix,
    ar.coefs = object$ar.coefficients,
    ma.coefs = object$ma.coefficients
  )

  # Do the forecast
  y.forecast[1] <- w$w.transpose %*% object$x[, ncol(object$x)]
  x[, 1] <- F %*% object$x[, ncol(object$x)] # + g$g %*% object$errors[length(object$errors)]

  if (h > 1) {
    for (t in 2:h) {
      x[, t] <- F %*% x[, (t - 1)]
      y.forecast[t] <- w$w.transpose %*% x[, (t - 1)]
    }
  }
  ## Make prediction intervals here
  lower.bounds <- upper.bounds <- matrix(NA, ncol = length(level), nrow = h)
  variance.multiplier <- numeric(h)
  variance.multiplier[1] <- 1
  if (h > 1) {
    for (j in 1:(h - 1)) {
      if (j == 1) {
        f.running <- diag(ncol(F))
      } else {
        f.running <- f.running %*% F
      }
      c.j <- w$w.transpose %*% f.running %*% g$g
      variance.multiplier[(j + 1)] <- variance.multiplier[j] + c.j^2
    }
  }

  variance <- object$variance * variance.multiplier
  # print(variance)
  st.dev <- sqrt(variance)
  for (i in seq_along(level)) {
    marg.error <- st.dev * abs(qnorm((100 - level[i]) / 200))
    lower.bounds[, i] <- y.forecast - marg.error
    upper.bounds[, i] <- y.forecast + marg.error
  }
  # Inv Box Cox transform if required
  if (!is.null(object$lambda)) {
    y.forecast <- InvBoxCox(
      y.forecast,
      lambda = object$lambda,
      biasadj = biasadj,
      fvar = variance
    )
    lower.bounds <- InvBoxCox(lower.bounds, object$lambda)
    if (object$lambda < 1) {
      lower.bounds <- pmax(lower.bounds, 0)
    }
    upper.bounds <- InvBoxCox(upper.bounds, object$lambda)
  }
  colnames(upper.bounds) <- colnames(lower.bounds) <- paste0(level, "%")

  forecast.object <- list(
    model = object,
    mean = future_msts(object$y, y.forecast),
    level = level,
    x = object$y,
    series = object$series,
    upper = future_msts(object$y, upper.bounds),
    lower = future_msts(object$y, lower.bounds),
    fitted = copy_msts(object$y, object$fitted.values),
    method = as.character(object),
    residuals = copy_msts(object$y, object$errors)
  )
  if (is.null(object$series)) {
    forecast.object$series <- deparse(object$call$y)
  }
  class(forecast.object) <- "forecast"
  forecast.object
}

#' @export
as.character.bats <- function(x, ...) {
  name <- "BATS("
  if (!is.null(x$lambda)) {
    name <- paste0(name, round(x$lambda, digits = 3))
  } else {
    name <- paste0(name, "1")
  }
  name <- paste0(name, ", {")
  if (!is.null(x$ar.coefficients)) {
    name <- paste0(name, length(x$ar.coefficients))
  } else {
    name <- paste0(name, "0")
  }
  name <- paste0(name, ",")
  if (!is.null(x$ma.coefficients)) {
    name <- paste0(name, length(x$ma.coefficients))
  } else {
    name <- paste0(name, "0")
  }
  name <- paste0(name, "}, ")
  if (!is.null(x$damping.parameter)) {
    name <- paste0(name, round(x$damping.parameter, digits = 3))
  } else {
    name <- paste0(name, "-")
  }
  name <- paste0(name, ", ")
  if (!is.null(x$seasonal.periods)) {
    name <- paste0(name, "{")
    for (i in x$seasonal.periods) {
      name <- paste0(name, i)
      if (i != x$seasonal.periods[length(x$seasonal.periods)]) {
        name <- paste0(name, ",")
      } else {
        name <- paste0(name, "})")
      }
    }
  } else {
    name <- paste0(name, "-)")
  }
  name
}
