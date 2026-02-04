#' Forecasting using ETS models
#'
#' Returns forecasts and other information for univariate ETS models.
#'
#' @inheritParams forecast.ts
#' @param object An object of class `ets`. Usually the result of a call
#' to [ets()].
#' @param simulate If `TRUE`, prediction intervals are produced by simulation rather
#' than using analytic formulae. Errors are assumed to be normally distributed.
#' @param bootstrap If `TRUE`, then prediction intervals are produced by
#' simulation using resampled errors (rather than normally distributed errors). Ignored if `innov` is not `NULL`.
#' @param innov Optional matrix of future innovations to be used in
#' simulations. Ignored if `simulate = FALSE`. If provided, this overrides the `bootstrap` argument. The matrix
#' should have `h` rows and `npaths` columns.
#' @param npaths Number of sample paths used in computing simulated prediction
#' intervals.
#' @param PI If `TRUE`, prediction intervals are produced, otherwise only point
#' forecasts are calculated. If `PI` is `FALSE`, then `level`,
#' `fan`, `simulate`, `bootstrap` and `npaths` are all
#' ignored.
#' @param ... Other arguments are ignored.
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman
#' @seealso [ets()], [ses()], [holt()], [hw()].
#' @keywords ts
#' @examples
#' fit <- ets(USAccDeaths)
#' plot(forecast(fit, h = 48))
#'
#' @export forecast.ets
#' @export
forecast.ets <- function(
  object,
  h = if (object$m > 1) 2 * object$m else 10,
  level = c(80, 95),
  fan = FALSE,
  simulate = FALSE,
  bootstrap = FALSE,
  innov = NULL,
  npaths = 5000,
  PI = TRUE,
  lambda = object$lambda,
  biasadj = NULL,
  ...
) {
  # Check inputs
  # if(h>2000 | h<=0)
  if (h <= 0) {
    stop("Forecast horizon out of bounds")
  }
  if (is.null(lambda)) {
    biasadj <- FALSE
  } else {
    if (is.null(biasadj)) {
      biasadj <- attr(lambda, "biasadj")
    }
    if (!is.logical(biasadj)) {
      warning("biasadj information not found, defaulting to FALSE.")
      biasadj <- FALSE
    }
  }
  if (!PI && !biasadj) {
    simulate <- bootstrap <- fan <- FALSE
    if (!biasadj) {
      npaths <- 2
    } # Just to avoid errors
    level <- 90
  }
  level <- getConfLevel(level, fan)
  # Order levels
  level <- sort(level)

  n <- length(object$x)
  damped <- as.logical(object$components[4])
  if (bootstrap) {
    simulate <- TRUE
  }

  if (simulate) {
    f <- pegelsfcast.C(
      h,
      object,
      level = level,
      bootstrap = bootstrap,
      npaths = npaths
    )
  } else if (
    object$components[1] == "A" &&
      object$components[2] %in% c("A", "N") &&
      object$components[3] %in% c("N", "A")
  ) {
    f <- class1(
      h,
      object$states[n + 1, ],
      object$components[2],
      object$components[3],
      damped,
      object$m,
      object$sigma2,
      object$par
    )
  } else if (
    object$components[1] == "M" &&
      object$components[2] %in% c("A", "N") &&
      object$components[3] %in% c("N", "A")
  ) {
    f <- class2(
      h,
      object$states[n + 1, ],
      object$components[2],
      object$components[3],
      damped,
      object$m,
      object$sigma2,
      object$par
    )
  } else if (
    object$components[1] == "M" &&
      object$components[3] == "M" &&
      object$components[2] != "M"
  ) {
    f <- class3(
      h,
      object$states[n + 1, ],
      object$components[2],
      object$components[3],
      damped,
      object$m,
      object$sigma2,
      object$par
    )
  } else {
    f <- pegelsfcast.C(
      h,
      object,
      level = level,
      bootstrap = bootstrap,
      npaths = npaths
    )
  }

  out <- list(
    model = object,
    mean = future_msts(object$x, f$mu),
    level = level,
    x = object$x
  )
  if (PI || biasadj) {
    if (!is.null(f$var)) {
      out$lower <- out$upper <- ts(matrix(NA, ncol = length(level), nrow = h))
      colnames(out$lower) <- colnames(out$upper) <- paste0(level, "%")
      for (i in seq_along(level)) {
        marg.error <- sqrt(f$var) * abs(qnorm((100 - level[i]) / 200))
        out$lower[, i] <- out$mean - marg.error
        out$upper[, i] <- out$mean + marg.error
      }
      out$lower <- copy_msts(out$mean, out$lower)
      out$upper <- copy_msts(out$mean, out$upper)
    } else if (!is.null(f$lower)) {
      out$lower <- copy_msts(out$mean, f$lower)
      out$upper <- copy_msts(out$mean, f$upper)
    } else if (PI) {
      warning("No prediction intervals for this model")
    } else if (any(biasadj)) {
      warning("No bias adjustment possible")
    }
  }

  out$fitted <- copy_msts(object$x, fitted(object))
  out$method <- object$method
  if (!is.null(object$series)) {
    out$series <- object$series
  } else {
    out$series <- object$call$y
  }
  out$residuals <- copy_msts(object$x, residuals(object))

  if (!is.null(lambda)) {
    # out$x <- InvBoxCox(object$x,lambda)
    # out$fitted <- InvBoxCox(out$fitted,lambda)
    out$mean <- InvBoxCox(out$mean, lambda, biasadj, out)
    if (PI) {
      # PI = TRUE
      out$lower <- InvBoxCox(out$lower, lambda)
      out$upper <- InvBoxCox(out$upper, lambda)
    }
  }
  if (!PI) {
    out$lower <- out$upper <- out$level <- NULL
  }

  structure(out, class = "forecast")
}

pegelsfcast.C <- function(h, obj, npaths, level, bootstrap, innov = NULL) {
  y.paths <- matrix(NA, nrow = npaths, ncol = h)
  obj$lambda <- NULL # No need to transform these here as we do it later.
  y.f <- .Call(
    etsforecast,
    as.double(obj$states[length(obj$x) + 1, ]),
    as.integer(obj$m),
    switch(obj$components[2], N = 0L, A = 1L, M = 2L),
    switch(obj$components[3], N = 0L, A = 1L, M = 2L),
    as.double(if (obj$components[4] == "FALSE") 1 else obj$par["phi"]),
    as.integer(h)
  )
  if (abs(y.f[1] + 99999) < 1e-7) {
    stop("Problem with multiplicative damped trend")
  }
  hilo <- simulate_forecast(
    object = obj,
    h = h,
    level = level,
    npaths = npaths,
    bootstrap = bootstrap,
    innov = innov
  )
  lower <- hilo$lower
  upper <- hilo$upper
  list(mu = y.f, lower = lower, upper = upper)
}

class1 <- function(
  h,
  last.state,
  trendtype,
  seasontype,
  damped,
  m,
  sigma2,
  par
) {
  p <- length(last.state)
  H <- matrix(c(1, rep(0, p - 1)), nrow = 1)
  if (seasontype == "A") {
    H[1, p] <- 1
  }
  if (trendtype == "A") {
    if (damped) {
      H[1, 2] <- par["phi"]
    } else {
      H[1, 2] <- 1
    }
  }
  F <- matrix(0, p, p)
  F[1, 1] <- 1
  if (trendtype == "A") {
    if (damped) {
      F[1, 2] <- F[2, 2] <- par["phi"]
    } else {
      F[1, 2] <- F[2, 2] <- 1
    }
  }
  if (seasontype == "A") {
    F[p - m + 1, p] <- 1
    F[(p - m + 2):p, (p - m + 1):(p - 1)] <- diag(m - 1)
  }
  G <- matrix(0, nrow = p, ncol = 1)
  G[1, 1] <- par["alpha"]
  if (trendtype == "A") {
    G[2, 1] <- par["beta"]
  }
  if (seasontype == "A") {
    G[3, 1] <- par["gamma"]
  }
  mu <- numeric(h)
  Fj <- diag(p)
  cj <- numeric(h - 1)
  if (h > 1) {
    for (i in seq_len(h - 1)) {
      mu[i] <- H %*% Fj %*% last.state
      cj[i] <- H %*% Fj %*% G
      Fj <- Fj %*% F
    }
    cj2 <- cumsum(cj^2)
    var <- sigma2 * c(1, 1 + cj2)
  } else {
    var <- sigma2
  }
  mu[h] <- H %*% Fj %*% last.state

  list(mu = mu, var = var, cj = cj)
}

class2 <- function(
  h,
  last.state,
  trendtype,
  seasontype,
  damped,
  m,
  sigma2,
  par
) {
  tmp <- class1(h, last.state, trendtype, seasontype, damped, m, sigma2, par)
  theta <- numeric(h)
  theta[1] <- tmp$mu[1]^2
  if (h > 1) {
    for (j in 2:h) {
      theta[j] <- tmp$mu[j]^2 +
        sigma2 * sum(tmp$cj[1:(j - 1)]^2 * theta[(j - 1):1])
    }
  }
  var <- (1 + sigma2) * theta - tmp$mu^2
  list(mu = tmp$mu, var = var)
}

class3 <- function(
  h,
  last.state,
  trendtype,
  seasontype,
  damped,
  m,
  sigma2,
  par
) {
  p <- length(last.state)
  H1 <- matrix(rep(1, 1 + (trendtype != "N")), nrow = 1)
  H2 <- matrix(c(rep(0, m - 1), 1), nrow = 1)
  if (trendtype == "N") {
    F1 <- 1
    G1 <- par["alpha"]
  } else {
    F1 <- rbind(c(1, 1), c(0, if (damped) par["phi"] else 1))
    G1 <- rbind(c(par["alpha"], par["alpha"]), c(par["beta"], par["beta"]))
  }
  F2 <- rbind(c(rep(0, m - 1), 1), cbind(diag(m - 1), rep(0, m - 1)))

  G2 <- matrix(0, m, m)
  G2[1, m] <- par["gamma"]
  Mh <- matrix(last.state[1:(p - m)]) %*%
    matrix(last.state[(p - m + 1):p], nrow = 1)
  Vh <- matrix(0, length(Mh), length(Mh))
  H21 <- H2 %x% H1
  F21 <- F2 %x% F1
  G21 <- G2 %x% G1
  K <- (G2 %x% F1) + (F2 %x% G1)
  mu <- var <- numeric(h)
  for (i in seq_len(h)) {
    mu[i] <- H1 %*% Mh %*% t(H2)
    var[i] <- (1 + sigma2) * H21 %*% Vh %*% t(H21) + sigma2 * mu[i]^2
    vecMh <- c(Mh)
    Vh <- F21 %*%
      Vh %*%
      t(F21) +
      sigma2 *
        (F21 %*%
          Vh %*%
          t(G21) +
          G21 %*% Vh %*% t(F21) +
          K %*% (Vh + vecMh %*% t(vecMh)) %*% t(K) +
          sigma2 * G21 %*% (3 * Vh + 2 * vecMh %*% t(vecMh)) %*% t(G21))
    Mh <- F1 %*% Mh %*% t(F2) + G1 %*% Mh %*% t(G2) * sigma2
  }
  list(mu = mu, var = var)
}

# ses <- function(x,h=10,level=c(80,95),fan=FALSE,...)
# {
#   fcast <- forecast(ets(x,"ANN"),h,level=level,fan=fan,...)
#   fcast$method <- "Simple exponential smoothing"
#   fcast$model$call <- match.call()
#   return(fcast)
# }

# holt <- function(x,h=10, damped=FALSE, level=c(80,95), fan=FALSE, ...)
# {
#   junk <- forecast(ets(x,"AAN",damped=damped),h,level=level,fan=fan,...)
#   if(damped)
#     junk$method <- "Damped Holt's method"
#   else
#     junk$method <- "Holt's method"
#   junk$model$call <- match.call()
#   return(junk)
# }

# hw <- function(x,h=2*frequency(x),seasonal="additive",damped=FALSE,level=c(80,95), fan=FALSE, ...)
# {
#   if(seasonal=="additive")
#   {
#     junk <- forecast(ets(x,"AAA",damped=damped),h,level=level,fan=fan,...)
#     junk$method <- "Holt-Winters' additive method"
#   }
#   else
#   {
#     junk <- forecast(ets(x,"MAM",damped=damped),h,level=level,fan=fan,...)
#     junk$method <- "Holt-Winters' multiplicative method"
#   }
#   junk$model$call <- match.call()
#   return(junk)
# }
