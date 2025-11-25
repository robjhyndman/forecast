test_that("simulated_seasonal", {
  fitting_functions <- c(
    auto.arima,
    ets,
    nnetar,
    tbats
  )
  for (i in seq_along(fitting_functions)) {
    # With Box-Cox
    fit <- fitting_functions[[i]](AirPassengers, lambda = 0.3)
    fc1 <- forecast(fit)
    fc2 <- forecast(fit, simulate = TRUE, npaths = 300)
    fc3 <- forecast(fit, bootstrap = TRUE, npaths = 300)
    expect_equal(fc1$mean, fc2$mean, tolerance = 1e-2)
    expect_equal(fc1$mean, fc3$mean, tolerance = 1e-2)
    expect_equal(fc1$lower, fc2$lower, tolerance = 1e-1)
    expect_equal(fc1$lower, fc3$lower, tolerance = 1e-1)
    expect_equal(fc1$upper, fc2$upper, tolerance = 1e-1)
    expect_equal(fc1$upper, fc3$upper, tolerance = 1e-1)
    # No Box-Cox
    fit <- fitting_functions[[i]](USAccDeaths)
    fc1 <- forecast(fit)
    fc2 <- forecast(fit, simulate = TRUE, npaths = 300)
    fc3 <- forecast(fit, bootstrap = TRUE, npaths = 300)
    expect_equal(fc1$mean, fc2$mean, tolerance = 1e-2)
    expect_equal(fc1$mean, fc3$mean, tolerance = 1e-2)
    expect_equal(fc1$lower, fc2$lower, tolerance = 1e-1)
    expect_equal(fc1$lower, fc3$lower, tolerance = 1e-1)
    expect_equal(fc1$upper, fc2$upper, tolerance = 1e-1)
    expect_equal(fc1$upper, fc3$upper, tolerance = 1e-1)
  }
})

test_that("simulated_nonseasonal", {
  fitting_functions <- c(
    ar,
    arfima,
    rw_model,
    spline_model
  )
  for (i in seq_along(fitting_functions)) {
    # With Box-Cox
    fit <- fitting_functions[[i]](Nile, lambda = 0.5)
    fc1 <- forecast(fit)
    fc2 <- forecast(fit, simulate = TRUE, npaths = 300)
    fc3 <- forecast(fit, bootstrap = TRUE, npaths = 300)
    expect_equal(fc1$mean, fc2$mean, tolerance = 1e-2)
    expect_equal(fc1$mean, fc3$mean, tolerance = 1e-2)
    expect_equal(fc1$lower, fc2$lower, tolerance = 1e-0)
    expect_equal(fc1$lower, fc3$lower, tolerance = 1e-0)
    expect_equal(fc1$upper, fc2$upper, tolerance = 1e-0)
    expect_equal(fc1$upper, fc3$upper, tolerance = 1e-0)
    # No Box-Cox
    fit <- fitting_functions[[i]](uspop)
    fc1 <- forecast(fit)
    fc2 <- forecast(fit, simulate = TRUE, npaths = 300)
    fc3 <- forecast(fit, bootstrap = TRUE, npaths = 300)
    expect_equal(fc1$mean, fc2$mean, tolerance = 1e-2)
    expect_equal(fc1$mean, fc3$mean, tolerance = 1e-2)
    expect_equal(fc1$lower, fc2$lower, tolerance = 1e1)
    expect_equal(fc1$lower, fc3$lower, tolerance = 1e1)
    expect_equal(fc1$upper, fc2$upper, tolerance = 1e1)
    expect_equal(fc1$upper, fc3$upper, tolerance = 1e1)
  }
})

testthat("simulated_ModelAR", {
  my_lm <- function(x, y) {
    structure(lsfit(x, y), class = "lsfit")
  }
  predict.lsfit <- function(object, newdata = NULL) {
    n <- length(object$qr$qt)
    if (is.null(newdata)) {
      z <- numeric(n)
      z[seq_len(object$qr$rank)] <- object$qr$qt[seq_len(object$qr$rank)]
      as.numeric(qr.qy(object$qr, z))
    } else {
      sum(object$coefficients * c(1, newdata))
    }
  }
  fit <- modelAR(
    lynx,
    p = 4,
    FUN = my_lm,
    predict.FUN = predict.lsfit,
    lambda = NULL
  )
  fc1 <- forecast(fit, PI = TRUE, npaths = 100)
  fc2 <- forecast(fit, PI = TRUE, bootstrap = TRUE, npaths = 100)
  expect_equal(fc1$mean, fc2$mean, tolerance = 1e-2)
  expect_equal(fc1$lower, fc2$lower, tolerance = 1e-0)
  expect_equal(fc1$upper, fc2$upper, tolerance = 1e-0)

  # No Box-Cox
  fit <- modelAR(
    lynx,
    p = 4,
    FUN = my_lm,
    predict.FUN = predict.lsfit,
    lambda = 0.5
  )
  fc1 <- forecast(fit, PI = TRUE, npaths = 100)
  fc2 <- forecast(fit, PI = TRUE, bootstrap = TRUE, npaths = 100)
  expect_equal(fc1$mean, fc2$mean, tolerance = 1e-2)
  expect_equal(fc1$lower, fc2$lower, tolerance = 1e-0)
  expect_equal(fc1$upper, fc2$upper, tolerance = 1e-0)
})
