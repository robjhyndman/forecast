#' @rdname forecast.bats
#' @export
forecast.tbats <- function(
  object,
  h,
  level = c(80, 95),
  fan = FALSE,
  simulate = FALSE,
  bootstrap = FALSE,
  innov = NULL,
  npaths = 5000,
  biasadj = NULL,
  ...
) {
  # Check if forecast.tbats called incorrectly
  if (inherits(object, "bats") && !inherits(object, "tbats")) {
    return(forecast.bats(object, h, level, fan, biasadj, ...))
  }

  # Set up the variables
  if (is.ts(object$y)) {
    ts.frequency <- frequency(object$y)
  } else if (!is.null(object$seasonal.periods)) {
    ts.frequency <- max(object$seasonal.periods)
  } else {
    ts.frequency <- 1
  }

  if (is.null(biasadj)) {
    if (!is.null(object$lambda)) {
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
  w <- makeTBATSWMatrix(
    smallPhi = object$damping.parameter,
    kVector = as.integer(object$k.vector),
    arCoefs = object$ar.coefficients,
    maCoefs = object$ma.coefficients,
    tau = as.integer(tau)
  )

  if (!is.null(object$seasonal.periods)) {
    gamma.bold <- matrix(0, nrow = 1, ncol = tau)
    updateTBATSGammaBold(
      gammaBold = gamma.bold,
      kVector = as.integer(object$k.vector),
      gammaOne = object$gamma.one.values,
      gammaTwo = object$gamma.two.values
    )
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
  updateTBATSGMatrix(
    g = g,
    gammaBold = gamma.bold,
    alpha = object$alpha,
    beta = object$beta.v
  )

  F <- makeTBATSFMatrix(
    alpha = object$alpha,
    beta = object$beta,
    small.phi = object$damping.parameter,
    seasonal.periods = object$seasonal.periods,
    k.vector = as.integer(object$k.vector),
    gamma.bold.matrix = gamma.bold,
    ar.coefs = object$ar.coefficients,
    ma.coefs = object$ma.coefficients
  )

  # Do the forecast
  y.forecast[1] <- w$w.transpose %*% object$x[, ncol(object$x)]
  x[, 1] <- F %*% object$x[, ncol(object$x)] # + g %*% object$errors[length(object$errors)]

  variance.multiplier <- numeric(h)
  variance.multiplier[1] <- 1
  if (h > 1) {
    for (t in 2L:h) {
      x[, t] <- F %*% x[, (t - 1)]
      y.forecast[t] <- w$w.transpose %*% x[, (t - 1)]
      j <- t - 1
      if (j == 1) {
        f.running <- diag(ncol(F))
      } else {
        f.running <- f.running %*% F
      }
      c.j <- w$w.transpose %*% f.running %*% g
      variance.multiplier[j + 1] <- variance.multiplier[j] + c.j^2
    }
  }
  variance <- object$variance * variance.multiplier

  if (!simulate && !bootstrap) {
    ## Make prediction intervals here
    lower.bounds <- upper.bounds <- matrix(NA, ncol = length(level), nrow = h)
    st.dev <- sqrt(variance)
    for (i in seq_along(level)) {
      marg.error <- st.dev * abs(qnorm((100 - level[i]) / 200))
      lower.bounds[, i] <- y.forecast - marg.error
      upper.bounds[, i] <- y.forecast + marg.error
    }
    # Inv Box Cox transform if required
    if (!is.null(object$lambda)) {
      lower.bounds <- InvBoxCox(lower.bounds, object$lambda)
      if (object$lambda < 1) {
        lower.bounds <- pmax(lower.bounds, 0)
      }
      upper.bounds <- InvBoxCox(upper.bounds, object$lambda)
    }
  } else {
    # Compute prediction intervals using simulations
    hilo <- simulate_forecast(
      object = object,
      h = h,
      level = level,
      npaths = npaths,
      bootstrap = bootstrap,
      innov = innov,
      ...
    )
    lower.bounds <- hilo$lower
    upper.bounds <- hilo$upper
  }
  # Inv Box Cox transform if required
  if (!is.null(object$lambda)) {
    y.forecast <- InvBoxCox(
      y.forecast,
      object$lambda,
      biasadj = biasadj,
      fvar = variance
    )
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
as.character.tbats <- function(x, ...) {
  name <- "TBATS("
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
    name <- paste0(name, round(x$damping.parameter, digits = 3), ",")
  } else {
    name <- paste0(name, "-,")
  }

  if (!is.null(x$seasonal.periods)) {
    name <- paste0(name, " {")
    M <- length(x$seasonal.periods)
    for (i in seq_len(M)) {
      name <- paste0(
        name,
        "<",
        round(x$seasonal.periods[i], 2),
        ",",
        x$k.vector[i],
        ">"
      )
      if (i < M) {
        name <- paste0(name, ", ")
      } else {
        name <- paste0(name, "})")
      }
    }
  } else {
    name <- paste0(name, "{-})")
  }
  name
}
