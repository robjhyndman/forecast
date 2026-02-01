###############################################
##### Forecasting Using Smoothing Splines #####
###############################################

# Optimal smoothing paramter denoted by beta
# lambda is Box-Cox parameter.

################# FUNCTIONS ##################

## Set up Sigma of order (n x n)
make.Sigma <- function(n, n0 = 0) {
  nn <- n + n0
  Sigma <- matrix(0, nrow = nn, ncol = nn)
  for (i in seq_len(nn)) {
    inn <- i:nn
    Sigma[i, inn] <- Sigma[inn, i] <- (i * i * (3 * (inn) - i)) / 6
  }
  Sigma / (n^3)
}

## Compute spline matrices
spline.matrices <- function(
  n,
  beta,
  cc = 1e2,
  n0 = 0,
  compute_inverse = TRUE,
  compute_P = TRUE
) {
  if (!compute_inverse) {
    compute_P <- FALSE
  }
  nn <- n + n0
  Sigma <- make.Sigma(n, n0)
  s <- cbind(rep(1, nn), seq(nn) / n)
  Omega <- cc * s %*% t(s) + Sigma / beta + diag(nn)
  maxO <- max(Omega)
  if (compute_inverse) {
    inv.Omega <- solve(Omega / maxO, tol = 1e-10) / maxO
  } else {
    inv.Omega <- NULL
  }
  if (compute_P) {
    P <- chol(inv.Omega)
  } else {
    P <- NULL
  }
  list(
    s = s,
    Sigma = Sigma,
    Omega = Omega,
    inv.Omega = inv.Omega,
    P = P
  )
}

## Compute smoothing splines
## Return -loglikelihood
# beta multiplied by 1e6 to avoid numerical difficulties in optimization
spline.loglik <- function(beta, y, cc = 1e2) {
  n <- length(y)
  mat <- spline.matrices(n, beta / 1e6, cc = cc)
  y.star <- mat$P %*% matrix(y)
  -log(det(mat$P)) + 0.5 * n * log(sum(y.star^2))
}

# Spline forecasting model

#' Cubic spline stochastic model
#'
#' Fits a state space model based on cubic smoothing splines.
#' The cubic smoothing spline model is equivalent to an ARIMA(0,2,2) model but
#' with a restricted parameter space. The advantage of the spline model over
#' the full ARIMA model is that it provides a smooth historical trend as well
#' as a linear forecast function. Hyndman, King, Pitrun, and Billah (2002) show
#' that the forecast performance of the method is hardly affected by the
#' restricted parameter space.
#'
#' @inheritParams Arima
#' @param method Method for selecting the smoothing parameter. If
#' `method = "gcv"`, the generalized cross-validation method from
#' [stats::smooth.spline()] is used. If `method = "mle"`, the
#' maximum likelihood method from Hyndman et al (2002) is used.
#' @return An object of class `spline_model`.
#' @author Rob J Hyndman
#' @seealso [stats::smooth.spline()], [stats::arima()], [holt()].
#' @references Hyndman, King, Pitrun and Billah (2005) Local linear forecasts
#' using cubic smoothing splines. \emph{Australian and New Zealand Journal of
#' Statistics}, \bold{47}(1), 87-99.
#' \url{https://robjhyndman.com/publications/splinefcast/}.
#' @keywords ts
#' @examples
#' fit <- spline_model(uspop)
#' fit
#' fit |> forecast() |> autoplot()
#'
#' @export
spline_model <- function(
  y,
  method = c("gcv", "mle"),
  lambda = NULL,
  biasadj = FALSE
) {
  method <- match.arg(method)
  seriesname <- deparse1(substitute(y))
  if (inherits(y, c("data.frame", "list", "matrix", "mts"))) {
    stop("y should be a univariate time series")
  }
  y <- as.ts(y)
  n <- length(y)
  tsattr <- tsp(y)
  orig.y <- y
  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
    attr(lambda, "biasadj") <- biasadj
  }

  # Find optimal beta using likelihood approach in Hyndman et al paper.
  if (method == "mle") {
    # Use only last 100 observations to get beta
    xx <- tail(y, min(100, n))
    beta.est <- 1e-6 *
      optimize(
        spline.loglik,
        interval = c(1e-6, 1e7),
        y = xx
      )$minimum
    # Compute spar which is equivalent to beta
    r <- 256 * smooth.spline(seq(n), y, spar = 0)$lambda
    lss <- beta.est / (1 - 1 / n)^3
    spar <- (log(lss / r) / log(256) + 1) / 3
    splinefit <- smooth.spline(seq(n), y, spar = spar)
    sfits <- splinefit$y
  } else {
    # Use GCV
    splinefit <- smooth.spline(seq(n), y, cv = FALSE, spar = NULL)
    sfits <- ts(splinefit$y)
    beta.est <- pmax(1e-7, splinefit$lambda * (1 - 1 / n)^3)
  }

  # Compute matrices for optimal beta
  mat <- spline.matrices(n, beta.est, compute_inverse = FALSE)
  maxO <- max(mat$Omega)

  # Get one-step predictors
  yfit <- e <- ts(rep(NA, n))
  if (n > 1000) {
    warning("Series too long to compute training set fits and residuals")
  } else {
    # This is probably grossly inefficient but I can't think of a better way
    for (i in seq_len(n - 1)) {
      idx <- seq(i)
      U <- mat$Omega[seq(i), i + 1]
      Oinv <- solve(mat$Omega[idx, idx] / maxO, tol = 1e-10) / maxO
      yfit[i + 1] <- t(U) %*% Oinv %*% y[idx]
      sd <- sqrt(mat$Omega[i + 1, i + 1] - t(U) %*% Oinv %*% U)
      e[i + 1] <- (y[i + 1] - yfit[i + 1]) / sd
    }
  }
  # Compute sigma^2
  sigma2 <- mean(e^2, na.rm = TRUE)
  if (!is.null(lambda)) {
    yfit <- InvBoxCox(yfit, lambda)
    sfits <- InvBoxCox(sfits, lambda)
  }
  tsp(e) <- tsp(yfit) <- tsp(sfits) <- tsattr

  structure(
    list(
      method = "Cubic Smoothing Spline",
      series = seriesname,
      y = orig.y,
      lambda = lambda,
      beta = beta.est * n^3,
      sigma2 = sigma2,
      fitted = sfits,
      residuals = e,
      onestepf = yfit,
      call = match.call()
    ),
    class = c("fc_model", "spline_model")
  )
}

#' @export
print.spline_model <- function(
  x,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  cat("Cubic spline stochastic model\n")
  cat("Call:", deparse(x$call), "\n")
  cat("Smoothing parameter:", format(x$beta, digits = digits), "\n")
  invisible(x)
}

#' Returns local linear forecasts and prediction intervals using cubic
#' smoothing splines estimated with [spline_model()].
#'
#' The cubic smoothing spline model is equivalent to an ARIMA(0,2,2) model but
#' with a restricted parameter space. The advantage of the spline model over
#' the full ARIMA model is that it provides a smooth historical trend as well
#' as a linear forecast function. Hyndman, King, Pitrun, and Billah (2002) show
#' that the forecast performance of the method is hardly affected by the
#' restricted parameter space.
#'
#' @param object An object of class `spline_model`, produced using [spline_model()].
#' @inheritParams forecast.ets
#' @return An object of class `forecast`.
#' @inheritSection forecast.ts forecast class
#' @author Rob J Hyndman
#' @seealso [spline_model()]
#' @references Hyndman, King, Pitrun and Billah (2005) Local linear forecasts
#' using cubic smoothing splines. \emph{Australian and New Zealand Journal of
#' Statistics}, \bold{47}(1), 87-99.
#' \url{https://robjhyndman.com/publications/splinefcast/}.
#' @keywords ts
#' @examples
#' fit <- spline_model(uspop)
#' fcast <- forecast(fit)
#' autoplot(fcast)
#' summary(fcast)
#'
#' @export
forecast.spline_model <- function(
  object,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = object$lambda,
  biasadj = attr(lambda, "biasadj"),
  simulate = FALSE,
  bootstrap = FALSE,
  innov = NULL,
  npaths = 5000,
  ...
) {
  n <- length(object$y)
  freq <- frequency(object$y)
  if (!is.null(lambda)) {
    y <- BoxCox(object$y, lambda)
  } else {
    y <- object$y
  }
  # Compute matrices for optimal beta
  mat <- spline.matrices(n, object$beta / n^3, compute_P = FALSE)
  newmat <- spline.matrices(n, object$beta / n^3, n0 = h, compute_inverse = FALSE)

  # Compute mean and var of forecasts
  U <- newmat$Omega[seq(n), n + seq(h)]
  Omega0 <- newmat$Omega[n + seq(h), n + seq(h)]
  Yhat <- t(U) %*% mat$inv.Omega %*% y
  sd <- sqrt(object$sigma2 * diag(Omega0 - t(U) %*% mat$inv.Omega %*% U))

  # Compute prediction intervals.
  level <- getConfLevel(level, fan)
  nconf <- length(level)
  startf <- tsp(y)[2] + 1 / freq
  lower <- upper <- ts(
    matrix(NA, nrow = h, ncol = nconf),
    start = startf,
    frequency = freq
  )
  if (simulate || bootstrap) {
    # Compute prediction intervals using simulations
    hilo <- simulate_forecast(
      object = object,
      h = h,
      level = level,
      npaths = npaths,
      bootstrap = bootstrap,
      innov = innov,
      lambda = lambda
    )
    lower <- hilo$lower
    upper <- hilo$upper
  } else {
    conf.factor <- qnorm(0.5 + 0.005 * level)
    for (i in seq_len(nconf)) {
      upper[, i] <- Yhat + conf.factor[i] * sd
      lower[, i] <- Yhat - conf.factor[i] * sd
    }
  }

  if (!is.null(lambda)) {
    Yhat <- InvBoxCox(
      Yhat,
      lambda = lambda,
      biasadj = biasadj,
      fvar = sd^2
    )
    if (!simulate && !bootstrap) {
      upper <- InvBoxCox(upper, lambda)
      lower <- InvBoxCox(lower, lambda)
    }
  }

  colnames(lower) <- colnames(upper) <- paste0(level, "%")

  structure(
    list(
      method = "Cubic Smoothing Spline",
      level = level,
      x = object$y,
      series = object$series,
      model = object,
      mean = ts(Yhat, frequency = freq, start = startf),
      upper = upper,
      lower = lower,
      fitted = object$fitted,
      residuals = object$residuals,
      onestepf = object$onestepf
    ),
    lambda = lambda,
    class = c("splineforecast", "forecast")
  )
}

#' @rdname forecast.spline_model
#' @inheritParams Arima
#' @export
splinef <- function(
  y,
  h = 10,
  level = c(80, 95),
  fan = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  method = c("gcv", "mle"),
  x = y
) {
  fit <- spline_model(x, method = method, lambda = lambda, biasadj = biasadj)
  forecast(fit, h = h, level = level, fan = fan)
}

#' @rdname plot.forecast
#'
#' @examples
#' fcast <- splinef(airmiles, h = 5)
#' plot(fcast)
#' autoplot(fcast)
#'
#' @export
plot.splineforecast <- function(x, fitcol = 2, type = "o", pch = 19, ...) {
  plot.forecast(x, type = type, pch = pch, ...)
  lines(x$fitted, col = fitcol)
}

#' @rdname is.forecast
#' @export
is.splineforecast <- function(x) {
  inherits(x, "splineforecast")
}

#' @rdname simulate.ets
#' @export
simulate.spline_model <- function(
  object,
  nsim = length(object$y),
  seed = NULL,
  future = TRUE,
  bootstrap = FALSE,
  innov = NULL,
  lambda = object$lambda,
  ...
) {
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
  } else {
    nsim <- length(innov)
  }

  if (bootstrap) {
    res <- na.omit(c(object$residuals) - mean(object$residuals, na.rm = TRUE))
    e <- sample(res, nsim, replace = TRUE)
  } else if (is.null(innov)) {
    se <- sqrt(object$sigma2)
    e <- rnorm(nsim, 0, se)
  } else {
    e <- innov
  }

  # Find starting position
  y <- object$y
  if (is.null(y)) {
    future <- FALSE
    if (nsim == 0L) {
      nsim <- 100
    }
    y <- 1
  }
  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
  }

  # Construct simulated ts
  nhistory <- min(length(object$y), 100)
  if (future) {
    y <- tail(y, nhistory)
  } else {
    y <- object$y[sample(nhistory - length(object$y)) + seq(nhistory)]
  }
  y <- c(y, rep(NA, nsim))
  for (i in nhistory + seq(nsim) - 1) {
    mat <- spline.matrices(i, object$beta / i^3, compute_P = FALSE)
    newmat <- spline.matrices(i, object$beta / i^3, n0 = 1, compute_inverse = FALSE)
    inv.Omega <- mat$inv.Omega
    Omega <- newmat$Omega
    U <- Omega[seq(i), i + 1]
    Omega0 <- Omega[i + 1, i + 1]
    Yhat <- t(U) %*% inv.Omega %*% y[seq(i)]
    sd <- sqrt(Omega0 - t(U) %*% inv.Omega %*% U)
    y[i + 1] <- Yhat + e[i - nhistory + 1] * sd
  }
  sim <- tail(y, nsim)
  if (!is.null(lambda)) {
    sim <- InvBoxCox(sim, lambda)
  }
  tspx <- tsp(object$y)
  ts(
    c(sim),
    start = if (future) tspx[2] + 1 / tspx[3] else tspx[1],
    frequency = tspx[3]
  )
}

#' @export
residuals.spline_model <- function(
  object,
  type = c("innovation", "response"),
  h = 1,
  ...
) {
  y <- getResponse(object)
  type <- match.arg(type)
  if (type == "innovation" && !is.null(object$lambda)) {
    res <- object$residuals
  } else {
    res <- y - fitted(object, h = h)
  }
  res <- ts(res)
  tsp(res) <- tsp(y)
  res
}
