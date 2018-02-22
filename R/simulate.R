#' Simulation from a time series model
#'
#' Returns a time series based on the model object \code{object}.
#'
#' With \code{simulate.Arima()}, the \code{object} should be produced by
#' \code{\link{Arima}} or \code{\link{auto.arima}}, rather than
#' \code{\link[stats]{arima}}. By default, the error series is assumed normally
#' distributed and generated using \code{\link[stats]{rnorm}}. If \code{innov}
#' is present, it is used instead. If \code{bootstrap=TRUE} and
#' \code{innov=NULL}, the residuals are resampled instead.
#'
#' When \code{future=TRUE}, the sample paths are conditional on the data. When
#' \code{future=FALSE} and the model is stationary, the sample paths do not
#' depend on the data at all. When \code{future=FALSE} and the model is
#' non-stationary, the location of the sample paths is arbitrary, so they all
#' start at the value of the first observation.
#'
#' @param object An object of class "\code{ets}", "\code{Arima}", "\code{ar}"
#' or "\code{nnetar}".
#' @param nsim Number of periods for the simulated series. Ignored if either
#' \code{xreg} or \code{innov} are not \code{NULL}.
#' @param seed Either \code{NULL} or an integer that will be used in a call to
#' \code{\link[base]{set.seed}} before simulating the time series. The default,
#' \code{NULL}, will not change the random generator state.
#' @param future Produce sample paths that are future to and conditional on the
#' data in \code{object}. Otherwise simulate unconditionally.
#' @param bootstrap Do simulation using resampled errors rather than normally
#' distributed errors or errors provided as \code{innov}.
#' @param innov A vector of innovations to use as the error series. Ignored if
#' \code{bootstrap==TRUE}. If not \code{NULL}, the value of \code{nsim} is set
#' to length of \code{innov}.
#' @param xreg New values of \code{xreg} to be used for forecasting. The value
#' of \code{nsim} is set to the number of rows of \code{xreg} if it is not
#' \code{NULL}.
#' @param lambda Box-Cox parameter. If not \code{NULL}, the simulated series is
#' transformed using an inverse Box-Cox transformation with parameter
#' \code{lamda}.
#' @param ... Other arguments, not currently used.
#' @return An object of class "\code{ts}".
#' @author Rob J Hyndman
#' @seealso \code{\link{ets}}, \code{\link{Arima}}, \code{\link{auto.arima}},
#' \code{\link{ar}}, \code{\link{arfima}}, \code{\link{nnetar}}.
#' @keywords ts
#' @examples
#' fit <- ets(USAccDeaths)
#' plot(USAccDeaths, xlim=c(1973,1982))
#' lines(simulate(fit, 36), col="red")
#'
#' @export
simulate.ets <- function(object, nsim=length(object$x), seed=NULL, future=TRUE, bootstrap=FALSE, innov=NULL, ...) {
  if (is.null(innov)) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      runif(1)
    }
    if (is.null(seed)) {
      RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
      R.seed <- get(".Random.seed", envir = .GlobalEnv)
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
  }
  else {
    nsim <- length(innov)
  }
  if (is.null(tsp(object$x))) {
    object$x <- ts(object$x, frequency = 1, start = 1)
  }

  if (future) {
    initstate <- object$state[length(object$x) + 1, ]
  } else { # choose a random starting point
    initstate <- object$state[sample(1:length(object$x), 1), ]
  }

  if (bootstrap) {
    res <- na.omit(c(object$residuals) - mean(object$residuals, na.rm = TRUE))
    e <- sample(res, nsim, replace = TRUE)
  }
  else if (is.null(innov)) {
    e <- rnorm(nsim, 0, sqrt(object$sigma))
  } else if (length(innov) == nsim) {
    e <- innov
  } else {
    stop("Length of innov must be equal to nsim")
  }
  if (object$components[1] == "M") {
    e <- pmax(-1, e)
  }
  tmp <- ts(.C(
    "etssimulate",
    as.double(initstate),
    as.integer(object$m),
    as.integer(switch(object$components[1], "A" = 1, "M" = 2)),
    as.integer(switch(object$components[2], "N" = 0, "A" = 1, "M" = 2)),
    as.integer(switch(object$components[3], "N" = 0, "A" = 1, "M" = 2)),
    as.double(object$par["alpha"]),
    as.double(ifelse(object$components[2] == "N", 0, object$par["beta"])),
    as.double(ifelse(object$components[3] == "N", 0, object$par["gamma"])),
    as.double(ifelse(object$components[4] == "FALSE", 1, object$par["phi"])),
    as.integer(nsim),
    as.double(numeric(nsim)),
    as.double(e),
    PACKAGE = "forecast"
  )[[11]], frequency = object$m, start = ifelse(future, tsp(object$x)[2] + 1 / tsp(object$x)[3], tsp(object$x)[1]))
  if (is.na(tmp[1])) {
    stop("Problem with multiplicative damped trend")
  }
  if (!is.null(object$lambda)) {
    tmp <- InvBoxCox(tmp, object$lambda)
  }
  return(tmp)
}


# Simulate ARIMA model starting with observed data x
# Some of this function is borrowed from the arima.sim() function in the stats package.
# Note that myarima.sim() does simulation conditional on the values of observed x, whereas
# arima.sim() is unconditional on any observed x.

myarima.sim <- function(model, n, x, e, ...) {
  start.innov <- residuals(model)
  innov <- e
  data <- x
  # Remove initial NAs
  first.nonmiss <- which(!is.na(x))[1]
  if (first.nonmiss > 1) {
    tsp.x <- tsp(x)
    start.x <- tsp.x[1] + (first.nonmiss - 1) / tsp.x[3]
    x <- window(x, start = start.x)
    start.innov <- window(start.innov, start = start.x)
  }
  model$x <- x
  n.start <- length(x)
  x <- ts(c(start.innov, innov), start = 1 - n.start, frequency = model$seasonal.period)
  flag.noadjust <- FALSE
  if (is.null(tsp(data))) {
    data <- ts(data, frequency = 1, start = 1)
  }
  if (!is.list(model)) {
    stop("'model' must be list")
  }
  if (n <= 0L) {
    stop("'n' must be strictly positive")
  }
  p <- length(model$ar)
  q <- length(model$ma)
  d <- 0
  D <- model$seasonal.difference
  m <- model$seasonal.period
  if (!is.null(ord <- model$order)) {
    if (length(ord) != 3L) {
      stop("'model$order' must be of length 3")
    }
    if (p != ord[1L]) {
      stop("inconsistent specification of 'ar' order")
    }
    if (q != ord[3L]) {
      stop("inconsistent specification of 'ma' order")
    }
    d <- ord[2L]
    if (d != round(d) || d < 0) {
      stop("number of differences must be a positive integer")
    }
  }
  if (p) {
    minroots <- min(Mod(polyroot(c(1, -model$ar))))
    if (minroots <= 1) {
      stop("'ar' part of model is not stationary")
    }
  }
  if (length(model$ma)) {
    # MA filtering
    x <- stats::filter(x, c(1, model$ma), method = "convolution", sides = 1L)
    x[seq_along(model$ma)] <- 0
  }
  ## AR "filtering"
  len.ar <- length(model$ar)

  if (length(model$ar) & (len.ar <= length(data))) {
    if ((D != 0) && (d != 0)) {
      diff.data <- diff(data, lag = 1, differences = d)
      diff.data <- diff(diff.data, lag = m, differences = D)
    }
    else if ((D != 0) && (d == 0)) {
      diff.data <- diff(data, lag = model$seasonal.period, differences = D)
    }
    else if ((D == 0) && (d != 0)) {
      diff.data <- diff(data, lag = 1, differences = d)
    } else {
      diff.data <- data
    }

    x.new.innovations <- x[(length(start.innov) + 1):length(x)]
    x.with.data <- c(diff.data, x.new.innovations)

    for (i in (length(diff.data) + 1):length(x.with.data))
    {
      lagged.x.values <- x.with.data[(i - len.ar):(i - 1)]
      ar.coefficients <- model$ar[length(model$ar):1]
      sum.multiplied.x <- sum((lagged.x.values * ar.coefficients)[abs(ar.coefficients) > .Machine$double.eps])
      x.with.data[i] <- x.with.data[i] + sum.multiplied.x
    }

    x.end <- x.with.data[(length(diff.data) + 1):length(x.with.data)]
    x <- ts(x.end, start = 1, frequency = model$seasonal.period)
    flag.noadjust <- TRUE
  }
  else if (length(model$ar)) # but data too short
  {
    # AR filtering for all other cases where AR is used.
    x <- stats::filter(x, model$ar, method = "recursive")
  }
  if ((d == 0) && (D == 0) && (flag.noadjust == FALSE)) # Adjust to ensure end matches approximately
  {
    # Last 20 diffs
    if (n.start >= 20) {
      xdiff <- (model$x - x[1:n.start])[n.start - (19:0)]
    } else {
      xdiff <- model$x - x[1:n.start]
    }
    # If all same sign, choose last
    if (all(sign(xdiff) == 1) | all(sign(xdiff) == -1)) {
      xdiff <- xdiff[length(xdiff)]
    } else { # choose mean.
      xdiff <- mean(xdiff)
    }
    x <- x + xdiff
  }
  if ((n.start > 0) && (flag.noadjust == FALSE)) {
    x <- x[-(1:n.start)]
  }


  ######## Undo all differences

  if ((D > 0) && (d == 0)) {
    # Seasonal undifferencing, if there is no regular differencing
    i <- length(data) - D * m + 1
    seasonal.xi <- data[i:length(data)]
    length.s.xi <- length(seasonal.xi)
    x <- diffinv(x, lag = m, differences = D, xi = seasonal.xi)[-(1:length.s.xi)]
  }
  else if ((d > 0) && (D == 0)) {
    # Regular undifferencing, if there is no seasonal differencing
    x <- diffinv(x, differences = d, xi = data[length(data) - (d:1) + 1])[-(1:d)]
  }
  else if ((d > 0) && (D > 0)) {
    # Undifferencing for where the differencing is both Seasonal and Non-Seasonal
    # Regular first
    delta.four <- diff(data, lag = m, differences = D)
    regular.xi <- delta.four[(length(delta.four) - D):length(delta.four)]
    x <- diffinv(x, differences = d, xi = regular.xi[length(regular.xi) - (d:1) + 1])[-(1:d)]

    # Then seasonal
    i <- length(data) - D * m + 1
    seasonal.xi <- data[i:length(data)]
    length.s.xi <- length(seasonal.xi)
    x <- diffinv(x, lag = m, differences = D, xi = seasonal.xi)
    x <- x[-(1:length.s.xi)]
  }

  x <- ts(x[1:n], frequency = frequency(data), start = tsp(data)[2] + 1 / tsp(data)[3])
  return(x)
}

#' @rdname simulate.ets
#' @export
simulate.Arima <- function(object, nsim=length(object$x), seed=NULL, xreg=NULL, future=TRUE, bootstrap=FALSE, innov=NULL, lambda=object$lambda, ...) {
  # Error check:
  if (object$arma[7] < 0) {
    stop("Value for seasonal difference is < 0. Must be >= 0")
  }
  else if ((sum(object$arma[c(3, 4, 7)]) > 0) && (object$arma[5] < 2)) {
    stop("Invalid value for seasonal period")
  }
  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    nsim <- nrow(xreg)
  }

  ####
  # Random Seed Code
  if (is.null(innov)) {
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      runif(1)
    }
    if (is.null(seed)) {
      RNGstate <- .Random.seed
    } else {
      R.seed <- .Random.seed
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
  }
  else {
    nsim <- length(innov)
  }

  ############# End Random seed code


  # Check for seasonal ARMA components and set flag accordingly. This will be used later in myarima.sim()
  flag.s.arma <- (sum(object$arma[c(3, 4)]) > 0)
  # Check for Seasonality in ARIMA model
  if (sum(object$arma[c(3, 4, 7)]) > 0) {
    # return(simulateSeasonalArima(object, nsim=nsim, seed=seed, xreg=xreg, future=future, bootstrap=bootstrap, ...))
    if (sum(object$model$phi) == 0) {
      ar <- NULL
    }
    else {
      ar <- as.double(object$model$phi)
    }
    if (sum(object$model$theta) == 0) {
      ma <- NULL
    }
    else {
      ma <- as.double(object$model$theta)
    }
    order <- c(length(ar), object$arma[6], length(ma))

    if (future) {
      model <- list(
        order = order, ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object),
        seasonal.difference = object$arma[7], seasonal.period = object$arma[5], flag.seasonal.arma = flag.s.arma,
        seasonal.order = object$arma[c(3, 7, 4)]
      )
    }
    else {
      model <- list(order = order, ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object))
    }
    flag.seasonal.diff <- (object$arma[7] > 0)
  }
  else {
    #### Non-Seasonal ARIMA specific code: Set up the model
    order <- object$arma[c(1, 6, 2)]
    if (order[1] > 0) {
      ar <- object$model$phi[1:order[1]]
    } else {
      ar <- NULL
    }
    if (order[3] > 0) {
      ma <- object$model$theta[1:order[3]]
    } else {
      ma <- NULL
    }
    if (object$arma[2] != length(ma)) {
      stop("MA length wrong")
    } else if (object$arma[1] != length(ar)) {
      stop("AR length wrong")
    }

    if (future) {
      model <- list(
        order = object$arma[c(1, 6, 2)], ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object),
        seasonal.difference = 0, flag.seasonal.arma = flag.s.arma, seasonal.order = c(0, 0, 0), seasonal.period = 1
      )
    }
    else {
      model <- list(order = object$arma[c(1, 6, 2)], ar = ar, ma = ma, sd = sqrt(object$sigma2), residuals = residuals(object))
    }
    flag.seasonal.diff <- FALSE
    ### End non-seasonal ARIMA specific code
  }


  x <- object$x <- getResponse(object)

  if (is.null(tsp(x))) {
    x <- ts(x, frequency = 1, start = 1)
  }

  if (!is.null(lambda)) {
    x <- BoxCox(x, lambda)
  }

  n <- length(x)
  if (bootstrap) {
    res <- na.omit(c(model$residuals) - mean(model$residuals, na.rm = TRUE))
    e <- sample(res, nsim, replace = TRUE)
  }
  else if (is.null(innov)) {
    e <- rnorm(nsim, 0, model$sd)
  } else if (length(innov) == nsim) {
    e <- innov
  } else {
    stop("Length of innov must be equal to nsim")
  }


  use.drift <- is.element("drift", names(object$coef))
  usexreg <- (!is.null(xreg) | use.drift)
  xm <- oldxm <- 0

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    if (nrow(xreg) < nsim) {
      stop("Not enough rows in xreg")
    } else {
      xreg <- xreg[1:nsim, ]
    }
  }
  if (use.drift) {
    # Remove existing drift column
    if (NCOL(xreg) == 1) {
      xreg <- NULL
    } else {
      xreg <- xreg[, !is.element(colnames(xreg), "drift"), drop = FALSE]
    }
    # Create new drift column for historical simulation
    dft <- as.matrix(1:nsim)
    # Adapt if future simulation
    if (future) {
      dft <- dft + n
    }
    # Add to xreg
    xreg <- cbind(drift = dft, xreg)
  }
  narma <- sum(object$arma[1L:4L])
  if (length(object$coef) > narma) {
    if (names(object$coef)[narma + 1L] == "intercept") {
      xreg <- cbind(intercept = rep(1, nsim), xreg)
      object$xreg <- cbind(intercept = rep(1, n), object$xreg)
    }
    if (!is.null(xreg)) {
      xm <- if (narma == 0) {
        drop(as.matrix(xreg) %*% object$coef)
      } else {
        drop(as.matrix(xreg) %*% object$coef[-(1L:narma)])
      }
      oldxm <- if (narma == 0) {
        drop(as.matrix(object$xreg) %*% object$coef)
      } else {
        drop(as.matrix(object$xreg) %*% object$coef[-(1L:narma)])
      }
    }
  }
  if (future) {
    sim <- myarima.sim(model, nsim, x - oldxm, e = e) + xm
  }
  else {
    if (flag.seasonal.diff) {
      zeros <- object$arma[5] * object$arma[7]
      sim <- arima.sim(model, nsim, innov = e)
      sim <- diffinv(sim, lag = object$arma[5], differences = object$arma[7])[-(1:zeros)]
      sim <- ts(tail(sim, nsim) + xm)
    }
    else {
      sim <- ts(tail(arima.sim(model, nsim, innov = e), nsim) + xm)
    }
    tsp(sim) <- tsp(x)
    # If model is non-stationary, then condition simulated data on first observation
    if (model$order[2] > 0 | flag.seasonal.diff) {
      sim <- sim - sim[1] + x[1]
    }
  }
  if (!is.null(lambda)) {
    sim <- InvBoxCox(sim, lambda)
  }


  return(sim)
}

#' @rdname simulate.ets
#' @export
simulate.ar <- function(object, nsim=object$n.used, seed=NULL, future=TRUE, bootstrap=FALSE, innov=NULL, ...) {
  if (is.null(innov)) {
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      runif(1)
    }
    if (is.null(seed)) {
      RNGstate <- .Random.seed
    } else {
      R.seed <- .Random.seed
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
  }
  else {
    nsim <- length(innov)
  }

  if (future) {
    model <- list(ar = object$ar, sd = sqrt(object$var.pred), residuals = object$resid, seasonal.difference = 0, seasonal.period = 1, flag.seasonal.arma = FALSE)
  }
  else {
    model <- list(ar = object$ar, sd = sqrt(object$var.pred), residuals = object$resid)
  }
  x.mean <- object$x.mean
  object$x <- getResponse(object)
  object$x <- object$x - x.mean
  if (bootstrap) {
    res <- na.omit(c(model$residuals) - mean(model$residuals, na.rm = TRUE))
    e <- sample(res, nsim, replace = TRUE)
  }
  else if (is.null(innov)) {
    e <- rnorm(nsim, 0, model$sd)
  } else if (length(innov) == nsim) {
    e <- innov
  } else {
    stop("Length of innov must be equal to nsim")
  }
  if (future) {
    return(myarima.sim(model, nsim, x = object$x, e = e) + x.mean)
  } else {
    return(arima.sim(model, nsim, innov = e) + x.mean)
  }
}

#' @rdname simulate.ets
#' @export
simulate.fracdiff <- function(object, nsim=object$n, seed=NULL, future=TRUE, bootstrap=FALSE, innov=NULL, ...) {
  x <- getResponse(object)

  # Strip initial and final missing values
  xx <- na.ends(x)
  n <- length(xx)

  # Remove mean
  meanx <- mean(xx)
  xx <- xx - meanx

  # Difference series (removes mean as well)
  y <- undo.na.ends(x, diffseries(xx, d = object$d))

  # Create ARMA model for differenced series
  arma <- Arima(
    y, order = c(length(object$ar), 0, length(object$ma)),
    include.mean = FALSE, fixed = c(object$ar, -object$ma)
  )

  # Simulate from ARMA model
  ysim <- simulate(arma, nsim, seed, future = future, bootstrap = bootstrap, innov = innov)

  # Undo differencing and add back mean
  return(unfracdiff(xx, ysim, n, nsim, object$d) + meanx)
}

#' @rdname simulate.ets
#' @export
simulate.nnetar <- function(object, nsim=length(object$x), seed=NULL, xreg=NULL, future=TRUE, bootstrap=FALSE, innov=NULL, lambda=object$lambda, ...) {
  if (is.null(innov)) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      runif(1)
    }
    if (is.null(seed)) {
      RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
      R.seed <- get(".Random.seed", envir = .GlobalEnv)
      set.seed(seed)
      RNGstate <- structure(seed, kind = as.list(RNGkind()))
      on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
  }
  else {
    nsim <- length(innov)
  }
  ## only future currently implemented
  if (!future) {
    warning("simulate.nnetar() currently only supports future=TRUE")
  }
  ## set simulation innovations
  if (bootstrap) {
    res <- na.omit(c(residuals(object, type = "innovation")))
    res <- res - mean(res)
    ## scale if appropriate
    if (!is.null(object$scalex$scale)) {
      res <- res / object$scalex$scale
    }
    e <- sample(res, nsim, replace = TRUE)
  }
  else if (is.null(innov)) {
    res <- na.omit(c(residuals(object, type = "innovation")))
    ## scale if appropriate
    if (!is.null(object$scalex$scale)) {
      res <- res / object$scalex$scale
    }
    e <- rnorm(nsim, 0, sd(res, na.rm = TRUE))
  }
  else if (length(innov) == nsim) {
    e <- innov / object$scalex$scale
  } else if (length(innov) == 1) {
    ## to pass innov=0 so simulation equals mean forecast
    e <- rep(innov, nsim) / object$scalex$scale
  } else {
    stop("Length of innov must be equal to nsim")
  }
  ##
  tspx <- tsp(object$x)
  # Check if xreg was used in fitted model
  if (is.null(object$xreg)) {
    if (!is.null(xreg)) {
      warning("External regressors were not used in fitted model, xreg will be ignored")
    }
    xreg <- NULL
  }
  else {
    if (is.null(xreg)) {
      stop("No external regressors provided")
    }
    xreg <- as.matrix(xreg)
    if (NCOL(xreg) != NCOL(object$xreg)) {
      stop("Number of external regressors does not match fitted model")
    }
    if (NROW(xreg) != nsim) {
      stop("Number of rows in xreg does not match nsim")
    }
  }
  xx <- object$x
  if (!is.null(lambda)) {
    xx <- BoxCox(xx, lambda)
  }
  # Check and apply scaling of fitted model
  if (!is.null(object$scalex)) {
    xx <- scale(xx, center = object$scalex$center, scale = object$scalex$scale)
    if (!is.null(xreg)) {
      xreg <- scale(xreg, center = object$scalexreg$center, scale = object$scalexreg$scale)
    }
  }
  ## Get lags used in fitted model
  lags <- object$lags
  maxlag <- max(lags)
  flag <- rev(tail(xx, n = maxlag))
  ## Simulate by iteratively forecasting and adding innovation
  path <- numeric(nsim)
  for (i in 1:nsim) {
    newdata <- c(flag[lags], xreg[i, ])
    if (any(is.na(newdata))) {
      stop("I can't simulate when there are missing values near the end of the series.")
    }
    path[i] <- mean(sapply(object$model, predict, newdata = newdata)) + e[i]
    flag <- c(path[i], flag[-maxlag])
  }
  ## Re-scale simulated points
  if (!is.null(object$scalex)) {
    path <- path * object$scalex$scale + object$scalex$center
  }
  ## Add ts properties
  path <- ts(path, start = tspx[2] + 1 / tspx[3], frequency = tspx[3])
  ## Back-transform simulated points
  if (!is.null(lambda)) {
    path <- InvBoxCox(path, lambda)
  }
  return(path)
}
