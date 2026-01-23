#' Exponential smoothing state space model
#'
#' Returns ets model applied to `y`.
#'
#' Based on the classification of methods as described in Hyndman et al (2008).
#'
#' The methodology is fully automatic. The only required argument for ets is
#' the time series. The model is chosen automatically if not specified. This
#' methodology performed extremely well on the M3-competition data. (See
#' Hyndman, et al, 2002, below.)
#'
#' @aliases print.ets summary.ets as.character.ets coef.ets tsdiag.ets
#'
#' @inheritParams forecast.ts
#' @param y a numeric vector or univariate time series of class `ts`
#' @param model Usually a three-character string identifying method using the
#' framework terminology of Hyndman et al. (2002) and Hyndman et al. (2008).
#' The first letter denotes the error type ("A", "M" or "Z"); the second letter
#' denotes the trend type ("N","A","M" or "Z"); and the third letter denotes
#' the season type ("N","A","M" or "Z"). In all cases, "N"=none, "A"=additive,
#' "M"=multiplicative and "Z"=automatically selected. So, for example, "ANN" is
#' simple exponential smoothing with additive errors, "MAM" is multiplicative
#' Holt-Winters' method with multiplicative errors, and so on.
#'
#' It is also possible for the model to be of class `ets`, and equal to
#' the output from a previous call to `ets`. In this case, the same model
#' is fitted to `y` without re-estimating any smoothing parameters. See
#' also the `use.initial.values` argument.
#' @param damped If `TRUE`, use a damped trend (either additive or
#' multiplicative). If `NULL`, both damped and non-damped trends will be
#' tried and the best model (according to the information criterion `ic`)
#' returned.
#' @param alpha Value of alpha. If `NULL`, it is estimated.
#' @param beta Value of beta. If `NULL`, it is estimated.
#' @param gamma Value of gamma. If `NULL`, it is estimated.
#' @param phi Value of phi. If `NULL`, it is estimated.
#' @param additive.only If `TRUE`, will only consider additive models. Default is
#' `FALSE`. When `lambda` is specified, `additive.only` is set to `TRUE`.
#' @param lambda Box-Cox transformation parameter. If `lambda = "auto"`,
#' then a transformation is automatically selected using `BoxCox.lambda`.
#' The transformation is ignored if NULL. Otherwise,
#' data transformed before model is estimated.
#' @param lower Lower bounds for the parameters (alpha, beta, gamma, phi). Ignored if `bounds = "admissible"`.
#' @param upper Upper bounds for the parameters (alpha, beta, gamma, phi). Ignored if `bounds = "admissible"`.
#' @param opt.crit Optimization criterion. One of "mse" (Mean Square Error),
#' "amse" (Average MSE over first `nmse` forecast horizons), "sigma"
#' (Standard deviation of residuals), "mae" (Mean of absolute residuals), or
#' "lik" (Log-likelihood, the default).
#' @param nmse Number of steps for average multistep MSE (1<=`nmse`<=30).
#' @param bounds Type of parameter space to impose: `"usual"` indicates
#' all parameters must lie between specified lower and upper bounds;
#' `"admissible"` indicates parameters must lie in the admissible space;
#' `"both"` (default) takes the intersection of these regions.
#' @param ic Information criterion to be used in model selection.
#' @param restrict If `TRUE` (default), the models with infinite variance
#' will not be allowed.
#' @param allow.multiplicative.trend If `TRUE`, models with multiplicative
#' trend are allowed when searching for a model. Otherwise, the model space
#' excludes them. This argument is ignored if a multiplicative trend model is
#' explicitly requested (e.g., using `model = "MMN"`).
#' @param use.initial.values If `TRUE` and `model` is of class
#' `"ets"`, then the initial values in the model are also not
#' re-estimated.
#' @param ... Other arguments are ignored.
#'
#' @return An object of class `ets`.
#'
#' The generic accessor functions `fitted.values` and `residuals`
#' extract useful features of the value returned by `ets` and associated
#' functions.
#' @author Rob J Hyndman
#' @seealso [stats::HoltWinters()], [rwf()], [Arima()].
#' @references Hyndman, R.J., Koehler, A.B., Snyder, R.D., and Grose, S. (2002)
#' "A state space framework for automatic forecasting using exponential
#' smoothing methods", \emph{International J. Forecasting}, \bold{18}(3),
#' 439--454.
#'
#' Hyndman, R.J., Akram, Md., and Archibald, B. (2008) "The admissible
#' parameter space for exponential smoothing models". \emph{Annals of
#' Statistical Mathematics}, \bold{60}(2), 407--426.
#'
#' Hyndman, R.J., Koehler, A.B., Ord, J.K., and Snyder, R.D. (2008)
#' \emph{Forecasting with exponential smoothing: the state space approach},
#' Springer-Verlag. \url{https://robjhyndman.com/expsmooth/}.
#' @keywords ts
#' @examples
#' fit <- ets(USAccDeaths)
#' plot(forecast(fit))
#'
#' @export
ets <- function(
  y,
  model = "ZZZ",
  damped = NULL,
  alpha = NULL,
  beta = NULL,
  gamma = NULL,
  phi = NULL,
  additive.only = FALSE,
  lambda = NULL,
  biasadj = FALSE,
  lower = c(rep(0.0001, 3), 0.8),
  upper = c(rep(0.9999, 3), 0.98),
  opt.crit = c("lik", "amse", "mse", "sigma", "mae"),
  nmse = 3,
  bounds = c("both", "usual", "admissible"),
  ic = c("aicc", "aic", "bic"),
  restrict = TRUE,
  allow.multiplicative.trend = FALSE,
  use.initial.values = FALSE,
  ...
) {
  opt.crit <- match.arg(opt.crit)
  bounds <- match.arg(bounds)
  ic <- match.arg(ic)
  seriesname <- deparse1(substitute(y))

  if (inherits(y, c("data.frame", "list", "matrix", "mts"))) {
    stop("y should be a univariate time series")
  }
  y <- as.ts(y)
  ny <- length(y)

  # Check if data is constant
  if (missing(model) && is.constant(y)) {
    return(ses(y, alpha = 0.99999, initial = "simple")$model)
  }

  orig.y <- y
  if (inherits(model, "ets") && is.null(lambda)) {
    lambda <- model$lambda
  }
  if (!is.null(lambda)) {
    y <- BoxCox(y, lambda)
    lambda <- attr(y, "lambda")
    attr(lambda, "biasadj") <- biasadj
    additive.only <- TRUE
  }

  if (nmse < 1 || nmse > 30) {
    stop("nmse out of range")
  }
  m <- max(1, frequency(y))
  if (abs(m - round(m)) > 1e-4) {
    warning(
      "Non-integer seasonal period. Only non-seasonal models will be considered."
    )
    m <- 1
  } else {
    m <- round(m)
  }

  if (any(upper < lower)) {
    stop("Lower limits must be less than upper limits")
  }

  # If model is an ets object, re-fit model to new data
  if (is.ets(model)) {
    # Prevent alpha being zero (to avoid divide by zero in the C code)
    alpha <- max(model$par["alpha"], 1e-10)
    beta <- model$par["beta"]
    if (is.na(beta)) {
      beta <- NULL
    }
    gamma <- model$par["gamma"]
    if (is.na(gamma)) {
      gamma <- NULL
    }
    phi <- model$par["phi"]
    if (is.na(phi)) {
      phi <- NULL
    }
    modelcomponents <- paste0(
      model$components[1],
      model$components[2],
      model$components[3]
    )
    damped <- (model$components[4] == "TRUE")
    if (use.initial.values) {
      errortype <- substr(modelcomponents, 1, 1)
      trendtype <- substr(modelcomponents, 2, 2)
      seasontype <- substr(modelcomponents, 3, 3)

      # Recompute errors from pegelsresid.C
      e <- pegelsresid.C(
        y,
        m,
        model$initstate,
        errortype,
        trendtype,
        seasontype,
        damped,
        alpha,
        beta,
        gamma,
        phi,
        nmse
      )

      # Compute error measures
      np <- length(model$par) + 1
      model$loglik <- -0.5 * e$lik
      model$aic <- e$lik + 2 * np
      model$bic <- e$lik + log(ny) * np
      model$aicc <- model$aic + 2 * np * (np + 1) / (ny - np - 1)
      model$mse <- e$amse[1]
      model$amse <- mean(e$amse)

      # Compute states, fitted values and residuals
      tsp.y <- tsp(y)
      model$states <- ts(
        e$states,
        frequency = tsp.y[3],
        start = tsp.y[1] - 1 / tsp.y[3]
      )
      colnames(model$states)[1] <- "l"
      if (trendtype != "N") {
        colnames(model$states)[2] <- "b"
      }
      if (seasontype != "N") {
        colnames(model$states)[
          (2 + (trendtype != "N")):ncol(model$states)
        ] <- paste0("s", 1:m)
      }
      model$fitted <- ts(e$fits, frequency = tsp.y[3], start = tsp.y[1])
      model$residuals <- ts(e$e, frequency = tsp.y[3], start = tsp.y[1])
      model$sigma2 <- sum(model$residuals^2, na.rm = TRUE) / (ny - np)
      model$x <- orig.y
      model$series <- seriesname
      if (!is.null(lambda)) {
        model$fitted <- InvBoxCox(
          model$fitted,
          lambda,
          biasadj,
          var(model$residuals)
        )
      }
      model$lambda <- lambda

      # Return model object
      return(model)
    } else {
      model <- modelcomponents
      if (missing(use.initial.values)) {
        message(
          "Model is being refit with current smoothing parameters but initial states are being re-estimated.\nSet 'use.initial.values=TRUE' if you want to re-use existing initial values."
        )
      }
    }
  }

  errortype <- substr(model, 1, 1)
  trendtype <- substr(model, 2, 2)
  seasontype <- substr(model, 3, 3)

  if (!errortype %in% c("M", "A", "Z")) {
    stop("Invalid error type")
  }
  if (!trendtype %in% c("N", "A", "M", "Z")) {
    stop("Invalid trend type")
  }
  if (!seasontype %in% c("N", "A", "M", "Z")) {
    stop("Invalid season type")
  }

  if (m < 1 || length(y) <= m) {
    # warning("I can't handle data with frequency less than 1. Seasonality will be ignored.")
    seasontype <- "N"
  }
  if (m == 1) {
    if (seasontype %in% c("A", "M")) {
      stop("Nonseasonal data")
    } else {
      substr(model, 3, 3) <- seasontype <- "N"
    }
  }
  if (m > 24) {
    if (seasontype %in% c("A", "M")) {
      stop("Frequency too high")
    } else if (seasontype == "Z") {
      warning(
        "I can't handle data with frequency greater than 24. Seasonality will be ignored. Try stlf() if you need seasonal forecasts."
      )
      substr(model, 3, 3) <- seasontype <- "N"
      # m <- 1
    }
  }

  # Check inputs
  if (restrict) {
    if (
      (errortype == "A" && (trendtype == "M" || seasontype == "M")) ||
        (errortype == "M" && trendtype == "M" && seasontype == "A") ||
        (additive.only &&
          (errortype == "M" || trendtype == "M" || seasontype == "M"))
    ) {
      stop("Forbidden model combination")
    }
  }

  data.positive <- (min(y, na.rm = TRUE) > 0)

  if (!data.positive && errortype == "M") {
    stop("Inappropriate model for data with negative or zero values")
  }

  if (!is.null(damped) && damped && trendtype == "N") {
    stop("Forbidden model combination")
  }

  n <- sum(!is.na(y))
  # Check we have enough data to fit a model
  npars <- 2L # alpha + l0
  if (trendtype %in% c("A", "M")) {
    npars <- npars + 2L
  } # beta + b0
  if (seasontype %in% c("A", "M")) {
    npars <- npars + m
  } # gamma + s
  if (!is.null(damped)) {
    npars <- npars + as.numeric(damped)
  }

  # Produce something non-optimized for tiny data sets
  if (n <= npars + 4L) {
    if (!is.null(damped) && damped) {
      warning("Not enough data to use damping")
    }
    if (seasontype %in% c("A", "M")) {
      fit <- try(
        HoltWintersZZ(
          orig.y,
          alpha = alpha,
          beta = beta,
          gamma = gamma,
          phi = phi,
          exponential = (trendtype == "M"),
          seasonal = if (seasontype != "A") "multiplicative" else "additive",
          lambda = lambda,
          biasadj = biasadj,
          warnings = FALSE
        ),
        silent = TRUE
      )
      if (!inherits(fit, "try-error")) {
        fit$call <- match.call()
        fit$method <- as.character(fit)
        fit$series <- deparse1(substitute(y))
        return(fit)
      } else {
        warning("Seasonal component could not be estimated")
      }
    }
    if (trendtype %in% c("A", "M")) {
      fit <- try(
        HoltWintersZZ(
          orig.y,
          alpha = alpha,
          beta = beta,
          gamma = FALSE,
          phi = phi,
          exponential = (trendtype == "M"),
          lambda = lambda,
          biasadj = biasadj,
          warnings = FALSE
        ),
        silent = TRUE
      )
      if (!inherits(fit, "try-error")) {
        fit$call <- match.call()
        fit$method <- as.character(fit)
        fit$series <- deparse1(substitute(y))
        return(fit)
      } else {
        warning("Trend component could not be estimated")
      }
    }
    if (trendtype == "N" && seasontype == "N") {
      fit <- try(
        HoltWintersZZ(
          orig.y,
          alpha = alpha,
          beta = FALSE,
          gamma = FALSE,
          lambda = lambda,
          biasadj = biasadj,
          warnings = FALSE
        ),
        silent = TRUE
      )
      if (!inherits(fit, "try-error")) {
        fit$call <- match.call()
        fit$method <- as.character(fit)
        fit$series <- deparse1(substitute(y))
        return(fit)
      }
    }
    # Try holt and ses and return best
    fit1 <- try(
      HoltWintersZZ(
        orig.y,
        alpha = alpha,
        beta = beta,
        gamma = FALSE,
        phi = phi,
        exponential = (trendtype == "M"),
        lambda = lambda,
        biasadj = biasadj,
        warnings = FALSE
      ),
      silent = TRUE
    )
    fit2 <- try(
      HoltWintersZZ(
        orig.y,
        alpha = alpha,
        beta = FALSE,
        gamma = FALSE,
        phi = phi,
        exponential = (trendtype == "M"),
        lambda = lambda,
        biasadj = biasadj,
        warnings = FALSE
      ),
      silent = TRUE
    )
    if (inherits(fit1, "try-error")) {
      fit <- fit2
    } else if (fit1$sigma2 < fit2$sigma2) {
      fit <- fit1
    } else {
      fit <- fit2
    }
    if (inherits(fit, "try-error")) {
      stop("Unable to estimate a model.")
    }
    fit$call <- match.call()
    fit$method <- as.character(fit)
    fit$series <- deparse1(substitute(y))
    return(fit)
  }

  # Fit model (assuming only one nonseasonal model)
  if (errortype == "Z") {
    errortype <- c("A", "M")
  }
  if (trendtype == "Z") {
    if (allow.multiplicative.trend) {
      trendtype <- c("N", "A", "M")
    } else {
      trendtype <- c("N", "A")
    }
  }
  if (seasontype == "Z") {
    seasontype <- c("N", "A", "M")
  }
  if (is.null(damped)) {
    damped <- c(TRUE, FALSE)
  }
  best.ic <- Inf
  for (i in seq_along(errortype)) {
    for (j in seq_along(trendtype)) {
      for (k in seq_along(seasontype)) {
        for (l in seq_along(damped)) {
          if (trendtype[j] == "N" && damped[l]) {
            next
          }
          if (restrict) {
            if (
              errortype[i] == "A" &&
                (trendtype[j] == "M" || seasontype[k] == "M")
            ) {
              next
            }
            if (
              errortype[i] == "M" && trendtype[j] == "M" && seasontype[k] == "A"
            ) {
              next
            }
            if (
              additive.only &&
                (errortype[i] == "M" ||
                  trendtype[j] == "M" ||
                  seasontype[k] == "M")
            ) {
              next
            }
          }
          if (!data.positive && errortype[i] == "M") {
            next
          }
          fit <- try(
            etsmodel(
              y,
              errortype[i],
              trendtype[j],
              seasontype[k],
              damped[l],
              alpha,
              beta,
              gamma,
              phi,
              lower = lower,
              upper = upper,
              opt.crit = opt.crit,
              nmse = nmse,
              bounds = bounds,
              ...
            ),
            silent = TRUE
          )
          if (inherits(fit, "try-error")) {
            fit.ic <- Inf
          } else {
            fit.ic <- switch(ic, aic = fit$aic, bic = fit$bic, aicc = fit$aicc)
          }
          if (!is.na(fit.ic) && fit.ic < best.ic) {
            model <- fit
            best.ic <- fit.ic
            best.e <- errortype[i]
            best.t <- trendtype[j]
            best.s <- seasontype[k]
            best.d <- damped[l]
          }
        }
      }
    }
  }
  if (best.ic == Inf) {
    stop("No model able to be fitted")
  }

  model$m <- m
  model$method <- paste0(
    "ETS(",
    best.e,
    ",",
    best.t,
    if (best.d) "d" else "",
    ",",
    best.s,
    ")"
  )
  model$series <- seriesname
  model$components <- c(best.e, best.t, best.s, best.d)
  model$call <- match.call()
  model$initstate <- model$states[1, ]
  np <- length(model$par)
  model$sigma2 <- sum(model$residuals^2, na.rm = TRUE) / (ny - np)
  model$x <- orig.y
  if (!is.null(lambda)) {
    model$fitted <- InvBoxCox(model$fitted, lambda, biasadj, model$sigma2)
  }

  model$lambda <- lambda
  structure(model, class = c("fc_model", "ets"))
}

#' @export
as.character.ets <- function(x, ...) {
  paste0(
    "ETS(",
    x$components[1],
    ",",
    x$components[2],
    if (x$components[4]) "d" else "",
    ",",
    x$components[3],
    ")"
  )
}

# myRequire <- function(libName) {

#   req.suc <- require(libName, quietly=TRUE, character.only=TRUE)
#   if(!req.suc) stop("The ",libName," package is not available.")

#   req.suc
# }

# getNewBounds <- function(par, lower, upper, nstate) {

#   myLower <- NULL
#   myUpper <- NULL

#   if("alpha" %in% names(par)) {
#     myLower <- c(myLower, lower[1])
#     myUpper <- c(myUpper, upper[1])
#   }
#   if("beta" %in% names(par)) {
#     myLower <- c(myLower, lower[2])
#     myUpper <- c(myUpper, upper[2])
#   }
#   if("gamma" %in% names(par)) {
#     myLower <- c(myLower, lower[3])
#     myUpper <- c(myUpper, upper[3])
#   }
#   if("phi" %in% names(par)) {
#     myLower <- c(myLower, lower[4])
#     myUpper <- c(myUpper, upper[4])
#   }

#   myLower <- c(myLower,rep(-1e8,nstate))
#   myUpper <- c(myUpper,rep(1e8,nstate))

#   list(lower=myLower, upper=myUpper)
# }

etsmodel <- function(
  y,
  errortype,
  trendtype,
  seasontype,
  damped,
  alpha = NULL,
  beta = NULL,
  gamma = NULL,
  phi = NULL,
  lower,
  upper,
  opt.crit,
  nmse,
  bounds,
  maxit = 2000,
  control = NULL,
  seed = NULL,
  trace = FALSE
) {
  tsp.y <- tsp(y)
  if (is.null(tsp.y)) {
    tsp.y <- c(1, length(y), 1)
  }
  if (seasontype != "N") {
    m <- tsp.y[3]
  } else {
    m <- 1
  }

  # Modify limits if alpha, beta or gamma have been specified.
  if (!is.null(alpha)) {
    upper[2] <- min(alpha, upper[2])
    upper[3] <- min(1 - alpha, upper[3])
  }
  if (!is.null(beta)) {
    lower[1] <- max(beta, lower[1])
  }
  if (!is.null(gamma)) {
    upper[1] <- min(1 - gamma, upper[1])
  }

  # Initialize smoothing parameters
  par <- initparam(
    alpha,
    beta,
    gamma,
    phi,
    trendtype,
    seasontype,
    damped,
    lower,
    upper,
    m,
    bounds
  )
  names(alpha) <- names(beta) <- names(gamma) <- names(phi) <- NULL
  par.noopt <- c(alpha = alpha, beta = beta, gamma = gamma, phi = phi)
  if (!is.null(par.noopt)) {
    par.noopt <- c(na.omit(par.noopt))
  }
  if (!is.na(par["alpha"])) {
    alpha <- par["alpha"]
  }
  if (!is.na(par["beta"])) {
    beta <- par["beta"]
  }
  if (!is.na(par["gamma"])) {
    gamma <- par["gamma"]
  }
  if (!is.na(par["phi"])) {
    phi <- par["phi"]
  }

  #    if(errortype=="M" | trendtype=="M" | seasontype=="M")
  #        bounds="usual"
  if (!check.param(alpha, beta, gamma, phi, lower, upper, bounds, m)) {
    cat(
      "Model: ETS(",
      errortype,
      ",",
      trendtype,
      if (damped) "d" else "",
      ",",
      seasontype,
      ")",
      sep = ""
    )
    stop("Parameters out of range")
  }

  # Initialize state
  init.state <- initstate(y, trendtype, seasontype)
  nstate <- length(init.state)
  par <- c(par, init.state)
  lower <- c(lower, rep(-Inf, nstate))
  upper <- c(upper, rep(Inf, nstate))

  np <- length(par)
  if (np >= length(y) - 1) {
    # Not enough data to continue
    return(list(
      aic = Inf,
      bic = Inf,
      aicc = Inf,
      mse = Inf,
      amse = Inf,
      fit = NULL,
      par = par,
      states = init.state
    ))
  }

  env <- etsTargetFunctionInitWrapper(
    par = par,
    y = y,
    nstate = nstate,
    errortype = errortype,
    trendtype = trendtype,
    seasontype = seasontype,
    damped = damped,
    par.noopt = par.noopt,
    lowerb = lower,
    upperb = upper,
    opt.crit = opt.crit,
    nmse = as.integer(nmse),
    bounds = bounds,
    m = m,
    pnames = names(par),
    pnames2 = names(par.noopt)
  )

  fred <- etsNelderMead(
    par,
    env,
    -Inf,
    sqrt(.Machine$double.eps),
    1.0,
    0.5,
    2.0,
    trace,
    maxit
  )
  fit.par <- fred$par
  names(fit.par) <- names(par)
  init.state <- fit.par[(np - nstate + 1):np]

  # Add extra state
  if (seasontype != "N") {
    init.state <- c(
      init.state,
      m * (seasontype == "M") - sum(init.state[(2 + (trendtype != "N")):nstate])
    )
  }

  if (!is.na(fit.par["alpha"])) {
    alpha <- fit.par["alpha"]
  }
  if (!is.na(fit.par["beta"])) {
    beta <- fit.par["beta"]
  }
  if (!is.na(fit.par["gamma"])) {
    gamma <- fit.par["gamma"]
  }
  if (!is.na(fit.par["phi"])) {
    phi <- fit.par["phi"]
  }

  e <- pegelsresid.C(
    y,
    m,
    init.state,
    errortype,
    trendtype,
    seasontype,
    damped,
    alpha,
    beta,
    gamma,
    phi,
    nmse
  )

  np <- np + 1
  ny <- length(y)
  aic <- e$lik + 2 * np
  bic <- e$lik + log(ny) * np
  aicc <- aic + 2 * np * (np + 1) / (ny - np - 1)

  mse <- e$amse[1]
  amse <- mean(e$amse)

  states <- ts(e$states, frequency = tsp.y[3], start = tsp.y[1] - 1 / tsp.y[3])
  colnames(states)[1] <- "l"
  if (trendtype != "N") {
    colnames(states)[2] <- "b"
  }
  if (seasontype != "N") {
    colnames(states)[(2 + (trendtype != "N")):ncol(states)] <- paste0("s", 1:m)
  }

  list(
    loglik = -0.5 * e$lik,
    aic = aic,
    bic = bic,
    aicc = aicc,
    mse = mse,
    amse = amse,
    fit = fred,
    residuals = ts(e$e, frequency = tsp.y[3], start = tsp.y[1]),
    fitted = ts(e$fits, frequency = tsp.y[3], start = tsp.y[1]),
    states = states,
    par = c(fit.par, par.noopt)
  )
}

etsTargetFunctionInitWrapper <- function(
  par,
  y,
  nstate,
  errortype,
  trendtype,
  seasontype,
  damped,
  par.noopt,
  lowerb,
  upperb,
  opt.crit,
  nmse,
  bounds,
  m,
  pnames,
  pnames2
) {
  names(par) <- pnames
  names(par.noopt) <- pnames2
  alpha <- c(par["alpha"], par.noopt["alpha"])["alpha"]
  if (is.na(alpha)) {
    stop("alpha problem!")
  }
  if (trendtype != "N") {
    beta <- c(par["beta"], par.noopt["beta"])["beta"]
    if (is.na(beta)) {
      stop("beta Problem!")
    }
  } else {
    beta <- NULL
  }
  if (seasontype != "N") {
    gamma <- c(par["gamma"], par.noopt["gamma"])["gamma"]
    if (is.na(gamma)) {
      stop("gamma Problem!")
    }
  } else {
    m <- 1
    gamma <- NULL
  }
  if (damped) {
    phi <- c(par["phi"], par.noopt["phi"])["phi"]
    if (is.na(phi)) {
      stop("phi Problem!")
    }
  } else {
    phi <- NULL
  }

  # determine which values to optimize and which ones are given by the user/not needed
  optAlpha <- !is.null(alpha)
  optBeta <- !is.null(beta)
  optGamma <- !is.null(gamma)
  optPhi <- !is.null(phi)

  givenAlpha <- FALSE
  givenBeta <- FALSE
  givenGamma <- FALSE
  givenPhi <- FALSE

  if (!is.null(par.noopt["alpha"]) && !is.na(par.noopt["alpha"])) {
    optAlpha <- FALSE
    givenAlpha <- TRUE
  }
  if (!is.null(par.noopt["beta"]) && !is.na(par.noopt["beta"])) {
    optBeta <- FALSE
    givenBeta <- TRUE
  }
  if (!is.null(par.noopt["gamma"]) && !is.na(par.noopt["gamma"])) {
    optGamma <- FALSE
    givenGamma <- TRUE
  }
  if (!is.null(par.noopt["phi"]) && !is.na(par.noopt["phi"])) {
    optPhi <- FALSE
    givenPhi <- TRUE
  }

  if (!damped) {
    phi <- 1
  }
  if (trendtype == "N") {
    beta <- 0
  }
  if (seasontype == "N") {
    gamma <- 0
  }

  #  cat("alpha: ", alpha)
  #  cat(" beta: ", beta)
  #  cat(" gamma: ", gamma)
  #  cat(" phi: ", phi, "\n")
  #
  #  cat("useAlpha: ", useAlpha)
  #  cat(" useBeta: ", useBeta)
  #  cat(" useGamma: ", useGamma)
  #  cat(" usePhi: ", usePhi, "\n")

  env <- new.env()

  res <- etsTargetFunctionInit(
    p_y = y,
    p_nstate = nstate,
    p_errortype = switch(errortype, A = 1L, M = 2L),
    p_trendtype = switch(trendtype, N = 0L, A = 1L, M = 2L),
    p_seasontype = switch(seasontype, N = 0L, A = 1L, M = 2L),
    p_damped = damped,
    p_lower = lowerb,
    p_upper = upperb,
    p_opt_crit = opt.crit,
    p_nmse = as.integer(nmse),
    p_bounds = bounds,
    p_m = m,
    p_optAlpha = optAlpha,
    p_optBeta = optBeta,
    p_optGamma = optGamma,
    p_optPhi = optPhi,
    p_givenAlpha = givenAlpha,
    p_givenBeta = givenBeta,
    p_givenGamma = givenGamma,
    p_givenPhi = givenPhi,
    p_alpha = alpha,
    p_beta = beta,
    p_gamma = gamma,
    p_phi = phi,
    p_rho = env
  )
  res
}

initparam <- function(
  alpha,
  beta,
  gamma,
  phi,
  trendtype,
  seasontype,
  damped,
  lower,
  upper,
  m,
  bounds
) {
  if (bounds == "admissible") {
    lower[1L:3L] <- lower[1L:3L] * 0
    upper[1L:3L] <- upper[1L:3L] * 0 + 1e-3
  } else if (any(lower > upper)) {
    stop("Inconsistent parameter boundaries")
  }

  # Select alpha
  if (is.null(alpha)) {
    alpha <- lower[1] + 0.2 * (upper[1] - lower[1]) / m
    if (alpha > 1 || alpha < 0) {
      alpha <- lower[1] + 2e-3
    }
    par <- c(alpha = alpha)
  } else {
    par <- numeric(0)
  }

  # Select beta
  if (trendtype != "N" && is.null(beta)) {
    # Ensure beta < alpha
    upper[2] <- min(upper[2], alpha)
    beta <- lower[2] + 0.1 * (upper[2] - lower[2])
    if (beta < 0 || beta > alpha) {
      beta <- alpha - 1e-3
    }
    par <- c(par, beta = beta)
  }

  # Select gamma
  if (seasontype != "N" && is.null(gamma)) {
    # Ensure gamma < 1-alpha
    upper[3] <- min(upper[3], 1 - alpha)
    gamma <- lower[3] + 0.05 * (upper[3] - lower[3])
    if (gamma < 0 || gamma > 1 - alpha) {
      gamma <- 1 - alpha - 1e-3
    }
    par <- c(par, gamma = gamma)
  }

  # Select phi
  if (damped && is.null(phi)) {
    phi <- lower[4] + .99 * (upper[4] - lower[4])
    if (phi < 0 || phi > 1) {
      phi <- upper[4] - 1e-3
    }
    par <- c(par, phi = phi)
  }

  par
}

check.param <- function(alpha, beta, gamma, phi, lower, upper, bounds, m) {
  if (bounds != "admissible") {
    if (!is.null(alpha)) {
      if (alpha < lower[1] || alpha > upper[1]) {
        return(0)
      }
    }
    if (!is.null(beta)) {
      if (beta < lower[2] || beta > alpha || beta > upper[2]) {
        return(0)
      }
    }
    if (!is.null(phi)) {
      if (phi < lower[4] || phi > upper[4]) {
        return(0)
      }
    }
    if (!is.null(gamma)) {
      if (gamma < lower[3] || gamma > 1 - alpha || gamma > upper[3]) {
        return(0)
      }
    }
  }
  if (bounds != "usual") {
    if (!admissible(alpha, beta, gamma, phi, m)) {
      return(0)
    }
  }
  1
}

initstate <- function(y, trendtype, seasontype) {
  if (seasontype != "N") {
    # Do decomposition
    m <- frequency(y)
    n <- length(y)
    y <- na.interp(y)
    if (n < 4) {
      stop("You've got to be joking (not enough data).")
    } else if (n < 3 * m) {
      # Fit simple Fourier model
      fouriery <- fourier(y, 1)
      fit <- tslm(y ~ trend + fouriery)
      if (seasontype == "A") {
        y.d <- list(
          seasonal = y - fit$coefficients[1] - fit$coefficients[2] * (1:n)
        )
      } else {
        # seasontype=="M". Biased method, but we only need a starting point
        y.d <- list(
          seasonal = y / (fit$coefficients[1] + fit$coefficients[2] * (1:n))
        )
      }
    } else {
      # n is large enough to do a decomposition
      y.d <- decompose(
        y,
        type = switch(seasontype, A = "additive", M = "multiplicative")
      )
    }

    init.seas <- rev(y.d$seasonal[2:m]) # initial seasonal component
    names(init.seas) <- paste0("s", 0:(m - 2))
    # Seasonally adjusted data
    if (seasontype == "A") {
      y.sa <- y - y.d$seasonal
    } else {
      init.seas <- pmax(init.seas, 1e-2) # We do not want negative seasonal indexes
      if (sum(init.seas) > m) {
        init.seas <- init.seas / sum(init.seas + 1e-2)
      }
      y.sa <- y / pmax(y.d$seasonal, 1e-2)
    }
  } else {
    # non-seasonal model
    m <- 1
    init.seas <- NULL
    y.sa <- y
  }

  maxn <- min(max(10, 2 * m), length(y.sa))
  if (trendtype == "N") {
    l0 <- mean(head(y.sa, maxn))
    b0 <- NULL
  } else {
    # Simple linear regression on seasonally adjusted data
    fit <- lsfit(seq(maxn), head(y.sa, maxn))
    if (trendtype == "A") {
      l0 <- fit$coefficients[1]
      b0 <- fit$coefficients[2]
      # If error type is "M", then we don't want l0+b0=0.
      # So perturb just in case.
      if (abs(l0 + b0) < 1e-8) {
        l0 <- l0 * (1 + 1e-3)
        b0 <- b0 * (1 - 1e-3)
      }
    } else {
      # if(trendtype=="M")
      l0 <- fit$coefficients[1] + fit$coefficients[2] # First fitted value
      if (abs(l0) < 1e-8) {
        l0 <- 1e-7
      }
      b0 <- (fit$coefficients[1] + 2 * fit$coefficients[2]) / l0 # Ratio of first two fitted values
      l0 <- l0 / b0 # First fitted value divided by b0
      if (abs(b0) > 1e10) {
        # Avoid infinite slopes
        b0 <- sign(b0) * 1e10
      }
      if (l0 < 1e-8 || b0 < 1e-8) {
        # Simple linear approximation didn't work.
        l0 <- max(y.sa[1], 1e-3)
        b0 <- max(y.sa[2] / y.sa[1], 1e-3)
      }
    }
  }

  names(l0) <- "l"
  if (!is.null(b0)) {
    names(b0) <- "b"
  }
  c(l0, b0, init.seas)
}

lik <- function(
  par,
  y,
  nstate,
  errortype,
  trendtype,
  seasontype,
  damped,
  par.noopt,
  lowerb,
  upperb,
  opt.crit,
  nmse,
  bounds,
  m,
  pnames,
  pnames2
) {
  names(par) <- pnames
  names(par.noopt) <- pnames2
  alpha <- c(par["alpha"], par.noopt["alpha"])["alpha"]
  if (is.na(alpha)) {
    stop("alpha problem!")
  }
  if (trendtype != "N") {
    beta <- c(par["beta"], par.noopt["beta"])["beta"]
    if (is.na(beta)) {
      stop("beta Problem!")
    }
  } else {
    beta <- NULL
  }
  if (seasontype != "N") {
    gamma <- c(par["gamma"], par.noopt["gamma"])["gamma"]
    if (is.na(gamma)) {
      stop("gamma Problem!")
    }
  } else {
    m <- 1
    gamma <- NULL
  }
  if (damped) {
    phi <- c(par["phi"], par.noopt["phi"])["phi"]
    if (is.na(phi)) {
      stop("phi Problem!")
    }
  } else {
    phi <- NULL
  }

  if (!check.param(alpha, beta, gamma, phi, lowerb, upperb, bounds, m)) {
    return(Inf)
  }

  np <- length(par)

  init.state <- par[(np - nstate + 1):np]
  # Add extra state
  if (seasontype != "N") {
    init.state <- c(
      init.state,
      m * (seasontype == "M") - sum(init.state[(2 + (trendtype != "N")):nstate])
    )
  }
  # Check states
  if (seasontype == "M") {
    seas.states <- init.state[-(1:(1 + (trendtype != "N")))]
    if (min(seas.states) < 0) {
      return(Inf)
    }
  }

  e <- pegelsresid.C(
    y,
    m,
    init.state,
    errortype,
    trendtype,
    seasontype,
    damped,
    alpha,
    beta,
    gamma,
    phi,
    nmse
  )

  if (is.na(e$lik)) {
    return(Inf)
  }
  if (e$lik < -1e10) {
    # Avoid perfect fits
    return(-1e10)
  }

  #      cat("lik: ", e$lik, "\n")
  #    points(alpha,e$lik,col=2)

  switch(
    opt.crit,
    lik = e$lik,
    mse = e$amse[1],
    amse = mean(e$amse),
    sigma = mean(e$e^2),
    mae = mean(abs(e$e))
  )
}

#' @export
print.ets <- function(x, ...) {
  cat(paste(x$method, "\n\n"))
  if (!is.null(x$call)) {
    cat("Call:", deparse(x$call), "", sep = "\n")
  }
  ncoef <- length(x$initstate)
  if (!is.null(x$lambda)) {
    cat("  Box-Cox transformation: lambda=", round(x$lambda, 4), "\n\n")
  }

  cat("  Smoothing parameters:\n")
  cat(paste("    alpha =", round(x$par["alpha"], 4), "\n"))
  if (x$components[2] != "N") {
    cat(paste("    beta  =", round(x$par["beta"], 4), "\n"))
  }
  if (x$components[3] != "N") {
    cat(paste("    gamma =", round(x$par["gamma"], 4), "\n"))
  }
  if (x$components[4] != "FALSE") {
    cat(paste("    phi   =", round(x$par["phi"], 4), "\n"))
  }

  cat("\n  Initial states:\n")
  cat(paste("    l =", round(x$initstate[1], 4), "\n"))
  if (x$components[2] != "N") {
    cat(paste("    b =", round(x$initstate[2], 4), "\n"))
  } else {
    x$initstate <- c(x$initstate[1], NA, x$initstate[2:ncoef])
    ncoef <- ncoef + 1
  }
  if (x$components[3] != "N") {
    cat("    s = ")
    if (ncoef <= 8) {
      cat(round(x$initstate[3:ncoef], 4))
    } else {
      cat(round(x$initstate[3:8], 4))
      cat("\n           ")
      cat(round(x$initstate[9:ncoef], 4))
    }
    cat("\n")
  }

  cat("\n  sigma:  ")
  cat(round(sqrt(x$sigma2), 4))
  if (!is.null(x$aic)) {
    stats <- c(x$aic, x$aicc, x$bic)
    names(stats) <- c("AIC", "AICc", "BIC")
    cat("\n\n")
    print(stats)
  }
  #    cat("\n  AIC:    ")
  #    cat(round(x$aic,4))
  #    cat("\n  AICc:   ")
  #    cat(round(x$aicc,4))
  #    cat("\n  BIC:    ")
  #    cat(round(x$bic,4))
}

pegelsresid.C <- function(
  y,
  m,
  init.state,
  errortype,
  trendtype,
  seasontype,
  damped,
  alpha,
  beta,
  gamma,
  phi,
  nmse
) {
  n <- length(y)
  p <- length(init.state)
  x <- numeric(p * (n + 1))
  x[1:p] <- init.state
  if (!damped) {
    phi <- 1
  }
  if (trendtype == "N") {
    beta <- 0
  }
  if (seasontype == "N") {
    gamma <- 0
  }

  res <- .Call(
    etscalc,
    as.double(y),
    as.double(x),
    as.integer(m),
    switch(errortype, A = 1L, M = 2L),
    switch(trendtype, N = 0L, A = 1L, M = 2L),
    switch(seasontype, N = 0L, A = 1L, M = 2L),
    as.double(alpha),
    as.double(beta),
    as.double(gamma),
    as.double(phi),
    as.integer(nmse)
  )
  tsp.y <- tsp(y)
  e <- ts(res$e)
  tsp(e) <- tsp.y

  list(
    lik = res$lik,
    amse = res$amse,
    e = e,
    fits = res$fitted,
    states = matrix(res$states, nrow = n + 1, ncol = p, byrow = TRUE)
  )
}

admissible <- function(alpha, beta, gamma, phi, m) {
  if (is.null(phi)) {
    phi <- 1
  }
  if (phi < 0 || phi > 1 + 1e-8) {
    return(0)
  }
  if (is.null(gamma)) {
    if (alpha < 1 - 1 / phi || alpha > 1 + 1 / phi) {
      return(0)
    }
    if (!is.null(beta)) {
      if (beta < alpha * (phi - 1) || beta > (1 + phi) * (2 - alpha)) {
        return(0)
      }
    }
  } else if (m > 1) {
    # Seasonal model
    if (is.null(beta)) {
      beta <- 0
    }
    if (gamma < max(1 - 1 / phi - alpha, 0) || gamma > 1 + 1 / phi - alpha) {
      return(0)
    }
    if (alpha < 1 - 1 / phi - gamma * (1 - m + phi + phi * m) / (2 * phi * m)) {
      return(0)
    }
    if (beta < -(1 - phi) * (gamma / m + alpha)) {
      return(0)
    }

    # End of easy tests. Now use characteristic equation
    P <- c(
      phi * (1 - alpha - gamma),
      alpha + beta - alpha * phi + gamma - 1,
      rep(alpha + beta - alpha * phi, m - 2),
      (alpha + beta - phi),
      1
    )
    roots <- polyroot(P)

    # cat("maxpolyroots: ", max(abs(roots)), "\n")

    if (max(abs(roots)) > 1 + 1e-10) {
      return(0)
    }
  }
  # Passed all tests
  1
}

### PLOT COMPONENTS

#' Plot components from ETS model
#'
#' Produces a plot of the level, slope and seasonal components from an ETS
#' model.
#'
#' `autoplot` will produce an equivalent plot as a ggplot object.
#'
#' @param x Object of class \dQuote{ets}.
#' @param object Object of class \dQuote{ets}. Used for ggplot graphics (S3
#' method consistency).
#' @param range.bars Logical indicating if each plot should have a bar at its
#' right side representing relative size. If `NULL`, automatic selection
#' takes place.
#' @param ... Other plotting parameters to affect the plot.
#' @return None. Function produces a plot
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#' @seealso [ets()]
#' @keywords hplot
#' @examples
#'
#' fit <- ets(USAccDeaths)
#' plot(fit)
#' plot(fit, plot.type = "single", ylab = "", col = 1:3)
#'
#' library(ggplot2)
#' autoplot(fit)
#'
#' @export
plot.ets <- function(x, ...) {
  if (!is.null(x$lambda)) {
    y <- BoxCox(x$x, x$lambda)
  } else {
    y <- x$x
  }
  if (x$components[3] == "N" && x$components[2] == "N") {
    plot(
      cbind(observed = y, level = x$states[, 1]),
      main = paste("Decomposition by", x$method, "method"),
      ...
    )
  } else if (x$components[3] == "N") {
    plot(
      cbind(observed = y, level = x$states[, 1], slope = x$states[, "b"]),
      main = paste("Decomposition by", x$method, "method"),
      ...
    )
  } else if (x$components[2] == "N") {
    plot(
      cbind(observed = y, level = x$states[, 1], season = x$states[, "s1"]),
      main = paste("Decomposition by", x$method, "method"),
      ...
    )
  } else {
    plot(
      cbind(
        observed = y,
        level = x$states[, 1],
        slope = x$states[, "b"],
        season = x$states[, "s1"]
      ),
      main = paste("Decomposition by", x$method, "method"),
      ...
    )
  }
}

#' @export
summary.ets <- function(object, ...) {
  class(object) <- c("summary.ets", class(object))
  object
}

#' @export
print.summary.ets <- function(x, ...) {
  NextMethod()
  cat("\nTraining set error measures:\n")
  print(accuracy(x))
}

#' @export
coef.ets <- function(object, ...) {
  object$par
}

#' @rdname fitted.Arima
#' @export
fitted.ets <- function(object, h = 1, ...) {
  if (h == 1) {
    object$fitted
  } else {
    hfitted(object = object, h = h, FUN = "ets", ...)
  }
}

#' @export
hfitted.ets <- function(object, h = 1, ...) {
  n <- length(object$x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n - h + 1)) {
    out[i + h - 1] <- .Call(
      etsforecast,
      as.double(object$states[i, ]),
      as.integer(object$m),
      switch(object$components[2], N = 0L, A = 1L, M = 2L),
      switch(object$components[3], N = 0L, A = 1L, M = 2L),
      as.double(if (object$components[4] == "FALSE") 1 else object$par["phi"]),
      as.integer(h)
    )[h]
  }
  out
}

#' @export
logLik.ets <- function(object, ...) {
  structure(object$loglik, df = length(object$par) + 1, class = "logLik")
}

#' @export
nobs.ets <- function(object, ...) {
  length(object$x)
}


#' Is an object a particular model type?
#'
#' Returns true if the model object is of a particular type
#'
#' @param x object to be tested
#' @export
is.ets <- function(x) {
  inherits(x, "ets")
}
