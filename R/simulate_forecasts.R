# Compute prediction intervals using simulations
simulate_forecast <- function(
  object,
  h,
  level = 80,
  npaths = 1000,
  bootstrap = FALSE,
  innov = NULL,
  lambda = NULL,
  ...
) {
  if (!is.null(innov)) {
    if (length(innov) != h * npaths) {
      stop("Incorrect number of innovations, need h*npaths values")
    }
    innov <- matrix(innov, nrow = h, ncol = npaths)
    if (bootstrap) {
      warning("innov being used, not bootstrap")
    }
    bootstrap <- FALSE
  }
  sim <- matrix(NA, nrow = npaths, ncol = h)
  for (i in seq_len(npaths)) {
    sim[i, ] <- simulate(
      object,
      nsim = h,
      bootstrap = bootstrap,
      lambda = lambda,
      innov = innov,
      future = TRUE,
      ...
    )
  }
  lower <- apply(sim, 2, quantile, 0.5 - level / 200, type = 8)
  upper <- apply(sim, 2, quantile, 0.5 + level / 200, type = 8)
  if (length(level) > 1L) {
    lower <- t(lower)
    upper <- t(upper)
  } else {
    lower <- matrix(lower, ncol = 1)
    upper <- matrix(upper, ncol = 1)
  }
  colnames(lower) <- colnames(upper) <- paste0(level, "%")
  y <- getResponse(object)
  tspy <- tsp(y)
  if (is.null(tspy)) {
    tspy <- c(1, length(y), 1)
  }
  m <- tspy[3]
  lower <- ts(lower, start = tspy[2] + 1 / m, frequency = m)
  upper <- ts(upper, start = tspy[2] + 1 / m, frequency = m)
  return(list(lower = lower, upper = upper))
}
