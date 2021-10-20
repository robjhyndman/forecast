#' @rdname forecast.bats
#' @export
forecast.tbats <- function(object, h, level = c(80, 95), fan = FALSE, biasadj = NULL, ...) {
  # Check if forecast.tbats called incorrectly
  if (identical(class(object), "bats")) {
    return(forecast.bats(object, h, level, fan, biasadj, ...))
  }

  # Set up the variables
  if (any(class(object$y) == "ts")) {
    ts.frequency <- frequency(object$y)
  } else {
    ts.frequency <- ifelse(!is.null(object$seasonal.periods), max(object$seasonal.periods), 1)
  }

  if (missing(h)) {
    if (is.null(object$seasonal.periods)) {
      h <- ifelse(ts.frequency == 1, 10, 2 * ts.frequency)
    } else {
      h <- 2 * max(object$seasonal.periods)
    }
  }
  else if (h <= 0) {
    stop("Forecast horizon out of bounds")
  }

  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 && max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 || max(level) > 99.99) {
      stop("Confidence limit out of range")
    }
  }

  if (!is.null(object$k.vector)) {
    tau <- 2 * sum(object$k.vector)
  } else {
    tau <- 0
  }
  x <- matrix(0, nrow = nrow(object$x), ncol = h)
  y.forecast <- numeric(h)
  if (!is.null(object$beta)) {
    adj.beta <- 1
  } else {
    adj.beta <- 0
  }

  # Set up the matrices
  w <- .Call("makeTBATSWMatrix", smallPhi_s = object$damping.parameter, kVector_s = as.integer(object$k.vector), arCoefs_s = object$ar.coefficients, maCoefs_s = object$ma.coefficients, tau_s = as.integer(tau), PACKAGE = "forecast")

  if (!is.null(object$seasonal.periods)) {
    gamma.bold <- matrix(0, nrow = 1, ncol = tau)
    .Call("updateTBATSGammaBold", gammaBold_s = gamma.bold, kVector_s = as.integer(object$k.vector), gammaOne_s = object$gamma.one.values, gammaTwo_s = object$gamma.two.values, PACKAGE = "forecast")
  } else {
    gamma.bold <- NULL
  }
  g <- matrix(0, nrow = (tau + 1 + adj.beta + object$p + object$q), ncol = 1)
  if (object$p != 0) {
    g[(1 + adj.beta + tau + 1), 1] <- 1
  }
  if (object$q != 0) {
    g[(1 + adj.beta + tau + object$p + 1), 1] <- 1
  }
  .Call("updateTBATSGMatrix", g_s = g, gammaBold_s = gamma.bold, alpha_s = object$alpha, beta_s = object$beta.v, PACKAGE = "forecast")

  # print(g)

  F <- makeTBATSFMatrix(alpha = object$alpha, beta = object$beta, small.phi = object$damping.parameter, seasonal.periods = object$seasonal.periods, k.vector = as.integer(object$k.vector), gamma.bold.matrix = gamma.bold, ar.coefs = object$ar.coefficients, ma.coefs = object$ma.coefficients)

  # Do the forecast
  y.forecast[1] <- w$w.transpose %*% object$x[, ncol(object$x)]
  x[, 1] <- F %*% object$x[, ncol(object$x)] # + g %*% object$errors[length(object$errors)]

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
      c.j <- w$w.transpose %*% f.running %*% g
      variance.multiplier[(j + 1)] <- variance.multiplier[j] + c.j^2
    }
  }

  variance <- object$variance * variance.multiplier
  # print(variance)
  st.dev <- sqrt(variance)
  for (i in 1:length(level)) {
    marg.error <- st.dev * abs(qnorm((100 - level[i]) / 200))
    lower.bounds[, i] <- y.forecast - marg.error
    upper.bounds[, i] <- y.forecast + marg.error
  }
  # Inv Box Cox transform if required
  if (!is.null(object$lambda)) {
    y.forecast <- InvBoxCox(y.forecast, object$lambda, biasadj, list(level = level, upper = upper.bounds, lower = lower.bounds))
    lower.bounds <- InvBoxCox(lower.bounds, object$lambda)
    if (object$lambda < 1) {
      lower.bounds <- pmax(lower.bounds, 0)
    }
    upper.bounds <- InvBoxCox(upper.bounds, object$lambda)
  }
  colnames(upper.bounds) <- colnames(lower.bounds) <- paste0(level, "%")

  forecast.object <- list(
    model = object, mean = future_msts(object$y, y.forecast),
    level = level, x = object$y, series = object$series,
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
  return(forecast.object)
}


#' @export
as.character.tbats <- function(x, ...) {
  name <- "TBATS("
  if (!is.null(x$lambda)) {
    name <- paste(name, round(x$lambda, digits = 3), sep = "")
  } else {
    name <- paste(name, "1", sep = "")
  }
  name <- paste(name, ", {", sep = "")
  if (!is.null(x$ar.coefficients)) {
    name <- paste(name, length(x$ar.coefficients), sep = "")
  } else {
    name <- paste(name, "0", sep = "")
  }
  name <- paste(name, ",", sep = "")
  if (!is.null(x$ma.coefficients)) {
    name <- paste(name, length(x$ma.coefficients), sep = "")
  } else {
    name <- paste(name, "0", sep = "")
  }
  name <- paste(name, "}, ", sep = "")
  if (!is.null(x$damping.parameter)) {
    name <- paste(name, round(x$damping.parameter, digits = 3), ",", sep = "")
  } else {
    name <- paste(name, "-,", sep = "")
  }

  if (!is.null(x$seasonal.periods)) {
    name <- paste(name, " {", sep = "")
    M <- length(x$seasonal.periods)
    for (i in 1:M) {
      name <- paste(name, "<", round(x$seasonal.periods[i], 2), ",", x$k.vector[i], ">", sep = "")
      if (i < M) {
        name <- paste(name, ", ", sep = "")
      } else {
        name <- paste(name, "})", sep = "")
      }
    }
  } else {
    name <- paste(name, "{-})", sep = "")
  }
  return(name)
}
