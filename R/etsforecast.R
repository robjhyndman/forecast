#' Forecasting using ETS models
#'
#' Returns forecasts and other information for univariate ETS models.
#'
#'
#' @param object An object of class "\code{ets}". Usually the result of a call
#' to \code{\link{ets}}.
#' @param h Number of periods for forecasting
#' @param level Confidence level for prediction intervals.
#' @param fan If TRUE, level is set to seq(51,99,by=3). This is suitable for
#' fan plots.
#' @param simulate If TRUE, prediction intervals are produced by simulation rather
#' than using analytic formulae. Errors are assumed to be normally distributed.
#' @param bootstrap If TRUE, then prediction intervals are produced by simulation using
#' resampled errors (rather than normally distributed errors).
#' @param npaths Number of sample paths used in computing simulated prediction
#' intervals.
#' @param PI If TRUE, prediction intervals are produced, otherwise only point
#' forecasts are calculated. If \code{PI} is FALSE, then \code{level},
#' \code{fan}, \code{simulate}, \code{bootstrap} and \code{npaths} are all
#' ignored.
#' @param lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise,
#' forecasts back-transformed via an inverse Box-Cox transformation.
#' @param biasadj Use adjusted back-transformed mean for Box-Cox
#' transformations. If TRUE, point forecasts and fitted values are mean
#' forecast. Otherwise, these points can be considered the median of the
#' forecast densities. By default, the value is taken from what was used when
#' fitting the model.
#' @param ... Other arguments.
#' @return An object of class "\code{forecast}".
#'
#' The function \code{summary} is used to obtain and print a summary of the
#' results, while the function \code{plot} produces a plot of the forecasts and
#' prediction intervals.
#'
#' The generic accessor functions \code{fitted.values} and \code{residuals}
#' extract useful features of the value returned by \code{forecast.ets}.
#'
#' An object of class \code{"forecast"} is a list containing at least the
#' following elements: \item{model}{A list containing information about the
#' fitted model} \item{method}{The name of the forecasting method as a
#' character string} \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals} \item{upper}{Upper
#' limits for prediction intervals} \item{level}{The confidence values
#' associated with the prediction intervals} \item{x}{The original time series
#' (either \code{object} itself or the time series used to create the model
#' stored as \code{object}).} \item{residuals}{Residuals from the fitted model.
#' For models with additive errors, the residuals are x - fitted values. For
#' models with multiplicative errors, the residuals are equal to x /(fitted
#' values) - 1.} \item{fitted}{Fitted values (one-step forecasts)}
#' @author Rob J Hyndman
#' @seealso \code{\link{ets}}, \code{\link{ses}}, \code{\link{holt}},
#' \code{\link{hw}}.
#' @keywords ts
#' @examples
#' fit <- ets(USAccDeaths)
#' plot(forecast(fit,h=48))
#'
#' @export
#' @export forecast.ets
forecast.ets <- function(object, h=ifelse(object$m > 1, 2 * object$m, 10),
                         level=c(80, 95), fan=FALSE, simulate=FALSE, bootstrap=FALSE, npaths=5000, PI=TRUE,
                         lambda=object$lambda, biasadj=NULL, ...) {
  # Check inputs
  # if(h>2000 | h<=0)
  if (h <= 0) {
    stop("Forecast horizon out of bounds")
  }
  if (is.null(lambda)) {
    biasadj <- FALSE
  }
  else {
    if (is.null(biasadj)) {
      biasadj <- attr(lambda, "biasadj")
    }
    if (!is.logical(biasadj)) {
      warning("biasadj information not found, defaulting to FALSE.")
      biasadj <- FALSE
    }
  }
  if (!PI & !biasadj) {
    simulate <- bootstrap <- fan <- FALSE
    if (!biasadj) {
      npaths <- 2
    } # Just to avoid errors
    level <- 90
  }
  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 & max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 | max(level) > 99.99) {
      stop("Confidence limit out of range")
    }
  }
  # Order levels
  level <- sort(level)

  n <- length(object$x)
  damped <- as.logical(object$components[4])
  if (bootstrap) {
    simulate <- TRUE
  }

  if (simulate) {
    f <- pegelsfcast.C(h, object, level = level, bootstrap = bootstrap, npaths = npaths)
  } else if (object$components[1] == "A" & is.element(object$components[2], c("A", "N")) & is.element(object$components[3], c("N", "A"))) {
    f <- class1(h, object$states[n + 1, ], object$components[2], object$components[3], damped, object$m, object$sigma2, object$par)
  } else if (object$components[1] == "M" & is.element(object$components[2], c("A", "N")) & is.element(object$components[3], c("N", "A"))) {
    f <- class2(h, object$states[n + 1, ], object$components[2], object$components[3], damped, object$m, object$sigma2, object$par)
  } else if (object$components[1] == "M" & object$components[3] == "M" & object$components[2] != "M") {
    f <- class3(h, object$states[n + 1, ], object$components[2], object$components[3], damped, object$m, object$sigma2, object$par)
  } else {
    f <- pegelsfcast.C(h, object, level = level, bootstrap = bootstrap, npaths = npaths)
  }

  tsp.x <- tsp(object$x)
  if (!is.null(tsp.x)) {
    start.f <- tsp(object$x)[2] + 1 / object$m
  } else {
    start.f <- length(object$x) + 1
  }
  out <- list(model = object, mean = ts(f$mu, frequency = object$m, start = start.f), level = level, x = object$x)
  if (PI | biasadj) {
    if (!is.null(f$var)) {
      out$lower <- out$upper <- ts(matrix(NA, ncol = length(level), nrow = h))
      colnames(out$lower) <- colnames(out$upper) <- paste(level, "%", sep = "")
      for (i in 1:length(level))
      {
        marg.error <- sqrt(f$var) * abs(qnorm((100 - level[i]) / 200))
        out$lower[, i] <- out$mean - marg.error
        out$upper[, i] <- out$mean + marg.error
      }
      tsp(out$lower) <- tsp(out$upper) <- tsp(out$mean)
    }
    else if (!is.null(f$lower)) {
      out$lower <- ts(f$lower)
      out$upper <- ts(f$upper)
      tsp(out$lower) <- tsp(out$upper) <- tsp(out$mean)
    }
    else if (PI) {
      warning("No prediction intervals for this model")
    } else if (any(biasadj)) {
      warning("No bias adjustment possible")
    }
  }

  out$fitted <- fitted(object)
  out$method <- object$method
  if (!is.null(object$series)) {
    out$series <- object$series
  }
  else {
    out$series <- object$call$y
  }
  out$residuals <- residuals(object)

  if (!is.null(lambda)) {
    # out$x <- InvBoxCox(object$x,lambda)
    # out$fitted <- InvBoxCox(out$fitted,lambda)
    out$mean <- InvBoxCox(out$mean, lambda, biasadj, out)
    if (PI) # PI = TRUE
    {
      out$lower <- InvBoxCox(out$lower, lambda)
      out$upper <- InvBoxCox(out$upper, lambda)
    }
  }
  if (!PI) {
    out$lower <- out$upper <- out$level <- NULL
  }

  return(structure(out, class = "forecast"))
}

pegelsfcast.C <- function(h, obj, npaths, level, bootstrap) {
  y.paths <- matrix(NA, nrow = npaths, ncol = h)
  obj$lambda <- NULL # No need to transform these here as we do it later.
  for (i in 1:npaths)
    y.paths[i, ] <- simulate.ets(obj, h, future = TRUE, bootstrap = bootstrap)
  y.f <- .C(
    "etsforecast",
    as.double(obj$state[length(obj$x) + 1, ]),
    as.integer(obj$m),
    as.integer(switch(obj$components[2], "N" = 0, "A" = 1, "M" = 2)),
    as.integer(switch(obj$components[3], "N" = 0, "A" = 1, "M" = 2)),
    as.double(ifelse(obj$components[4] == "FALSE", 1, obj$par["phi"])),
    as.integer(h),
    as.double(numeric(h)),
    PACKAGE = "forecast"
  )[[7]]
  if (abs(y.f[1] + 99999) < 1e-7) {
    stop("Problem with multiplicative damped trend")
  }

  lower <- apply(y.paths, 2, quantile, 0.5 - level / 200, type = 8, na.rm = TRUE)
  upper <- apply(y.paths, 2, quantile, 0.5 + level / 200, type = 8, na.rm = TRUE)
  if (length(level) > 1) {
    lower <- t(lower)
    upper <- t(upper)
  }
  return(list(mu = y.f, lower = lower, upper = upper))
}

class1 <- function(h, last.state, trendtype, seasontype, damped, m, sigma2, par) {
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
    for (i in 1:(h - 1))
    {
      mu[i] <- H %*% Fj %*% last.state
      cj[i] <- H %*% Fj %*% G
      Fj <- Fj %*% F
    }
    cj2 <- cumsum(cj ^ 2)
    var <- sigma2 * c(1, 1 + cj2)
  }
  else {
    var <- sigma2
  }
  mu[h] <- H %*% Fj %*% last.state

  return(list(mu = mu, var = var, cj = cj))
}

class2 <- function(h, last.state, trendtype, seasontype, damped, m, sigma2, par) {
  tmp <- class1(h, last.state, trendtype, seasontype, damped, m, sigma2, par)
  theta <- numeric(h)
  theta[1] <- tmp$mu[1] ^ 2
  if (h > 1) {
    for (j in 2:h)
      theta[j] <- tmp$mu[j] ^ 2 + sigma2 * sum(tmp$cj[1:(j - 1)] ^ 2 * theta[(j - 1):1])
  }
  var <- (1 + sigma2) * theta - tmp$mu ^ 2
  return(list(mu = tmp$mu, var = var))
}

class3 <- function(h, last.state, trendtype, seasontype, damped, m, sigma2, par) {
  p <- length(last.state)
  H1 <- matrix(rep(1, 1 + (trendtype != "N")), nrow = 1)
  H2 <- matrix(c(rep(0, m - 1), 1), nrow = 1)
  if (trendtype == "N") {
    F1 <- 1
    G1 <- par["alpha"]
  }
  else {
    F1 <- rbind(c(1, 1), c(0, ifelse(damped, par["phi"], 1)))
    G1 <- rbind(c(par["alpha"], par["alpha"]), c(par["beta"], par["beta"]))
  }
  F2 <- rbind(c(rep(0, m - 1), 1), cbind(diag(m - 1), rep(0, m - 1)))

  G2 <- matrix(0, m, m)
  G2[1, m] <- par["gamma"]
  Mh <- matrix(last.state[1:(p - m)]) %*% matrix(last.state[(p - m + 1):p], nrow = 1)
  Vh <- matrix(0, length(Mh), length(Mh))
  H21 <- H2 %x% H1
  F21 <- F2 %x% F1
  G21 <- G2 %x% G1
  K <- (G2 %x% F1) + (F2 %x% G1)
  mu <- var <- numeric(h)
  for (i in 1:h)
  {
    mu[i] <- H1 %*% Mh %*% t(H2)
    var[i] <- (1 + sigma2) * H21 %*% Vh %*% t(H21) + sigma2 * mu[i] ^ 2
    vecMh <- c(Mh)
    Vh <- F21 %*% Vh %*% t(F21) + sigma2 * (F21 %*% Vh %*% t(G21) + G21 %*% Vh %*% t(F21) +
      K %*% (Vh + vecMh %*% t(vecMh)) %*% t(K) + sigma2 * G21 %*% (3 * Vh + 2 * vecMh %*% t(vecMh)) %*% t(G21))
    Mh <- F1 %*% Mh %*% t(F2) + G1 %*% Mh %*% t(G2) * sigma2
  }
  return(list(mu = mu, var = var))
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
