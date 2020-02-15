# A unit test for re-fitting models
if (require(testthat)) {
  context("Re-fitting models")

  test_that("tests for re-fitting models", {
    # arima
    fit <- Arima(mdeaths, c(1, 0, 0), c(2, 0, 0), include.mean = FALSE, include.drift = TRUE)

    refit <- Arima(fdeaths, model = fit)
    expect_true(identical(fit$coef, refit$coef))
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))

    refit_same <- Arima(mdeaths, model = fit)
    expect_true(identical(fit$coef, refit_same$coef))
    expect_true(identical(fit$x, refit_same$x))
    expect_true(all.equal(fit$fitted, refit_same$fitted))
    expect_true(all.equal(fit$residuals, refit_same$residuals))

    # arfima
    fit <- arfima(mdeaths)

    refit <- arfima(fdeaths, model = fit)
    expect_true(identical(fit$ar, refit$ar))
    expect_true(identical(fit$ma, refit$ma))
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))

    refit_same <- arfima(mdeaths, model = fit)
    expect_true(identical(fit$ar, refit_same$ar))
    expect_true(identical(fit$ma, refit_same$ma))
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted, refit_same$fitted))
    expect_true(identical(fit$residuals, refit_same$residuals))

    # dshw
    fit <- dshw(mdeaths, period1 = 4, period2 = 12)

    refit <- dshw(fdeaths, model = fit)
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))

    refit_same <- dshw(mdeaths, model = fit)
    expect_true(identical(fit$model, refit_same$model))
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted, refit_same$fitted))
    expect_true(identical(fit$residuals, refit_same$residuals))

    # ets
    fit <- ets(mdeaths)

    refit <- ets(fdeaths, model = fit, use.initial.values = TRUE)
    expect_true(identical(fit$fit, refit$fit))
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))

    refit_same <- ets(mdeaths, model = fit, use.initial.values = TRUE)
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted, refit_same$fitted))
    expect_true(identical(residuals(fit), residuals(refit_same)))

    # stlm
    fit <- stlm(mdeaths)

    refit <- stlm(fdeaths, model = fit)
    expect_true(identical(fit$model$par, refit$model$par))
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))

    refit_same <- stlm(mdeaths, model = fit)
    expect_true(identical(fit$model$par, refit_same$model$par))
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted, refit_same$fitted))
    expect_true(identical(fit$residuals, refit_same$residuals))

    # bats
    fit <- bats(mdeaths)

    refit <- bats(fdeaths, model = fit)
    expect_true(identical(fit$parameters, refit$parameters))
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted.values, refit$fitted.values))
    expect_false(identical(residuals(fit), residuals(refit)))

    refit_same <- bats(mdeaths, model = fit)
    expect_true(identical(fit$model$par, refit_same$model$par))
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted.values, refit_same$fitted.values))
    expect_true(identical(residuals(fit), residuals(refit_same)))

    # tbats
    fit <- tbats(mdeaths)

    refit <- tbats(fdeaths, model = fit)
    expect_true(identical(fit$parameters, refit$parameters))
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted.values, refit$fitted.values))
    expect_false(identical(residuals(fit), residuals(refit)))

    refit_same <- tbats(mdeaths, model = fit)
    expect_true(identical(fit$model$par, refit_same$model$par))
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted.values, refit_same$fitted.values))
    expect_true(identical(residuals(fit), residuals(refit_same)))

    # nnetar
    fit <- nnetar(mdeaths)

    refit <- nnetar(fdeaths, model = fit)
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))

    refit_same <- nnetar(mdeaths, model = fit)
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted, refit_same$fitted))
    expect_true(identical(residuals(fit), residuals(refit_same)))

    # forecast.ts
    fit <- forecast(mdeaths)

    refit <- forecast(fdeaths, model = fit, use.initial.values = TRUE)
    expect_false(identical(fit$x, refit$x))
    expect_false(identical(fit$fitted, refit$fitted))
    expect_false(identical(fit$residuals, refit$residuals))

    refit_same <- forecast(mdeaths, model = fit, use.initial.values = TRUE)
    expect_true(identical(fit$x, refit_same$x))
    expect_true(identical(fit$fitted, refit_same$fitted))
    expect_true(identical(residuals(fit), residuals(refit_same)))
  })
}
