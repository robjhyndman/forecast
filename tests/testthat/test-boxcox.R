# A unit test for boxcox transformations
test_that("tests for biasadj automatically set based on model fit", {
  # lm
  fit <- tslm(USAccDeaths ~ trend, lambda = 0.5, biasadj = TRUE)
  expect_true(all.equal(forecast(fit), forecast(fit, biasadj = TRUE)))

  # HoltWintersZZ
  fit <- ses(USAccDeaths, initial = "simple", lambda = 0.5, biasadj = TRUE)
  expect_true(all.equal(forecast(fit), forecast(fit, biasadj = TRUE)))

  # arfima
  x <- fracdiff::fracdiff.sim(100, ma = -.4, d = .3)$series
  fit <- arfima(x)
  expect_true(all.equal(forecast(fit), forecast(fit, biasadj = TRUE)))

  #arima
  fit1 <- Arima(
    USAccDeaths,
    order = c(0, 1, 1),
    seasonal = c(0, 1, 1),
    lambda = 0.5,
    biasadj = TRUE
  )
  fit2 <- auto.arima(
    USAccDeaths,
    max.p = 0,
    max.d = 1,
    max.q = 1,
    max.P = 0,
    max.D = 1,
    max.Q = 1,
    lambda = 0.5,
    biasadj = TRUE
  )
  expect_true(all.equal(forecast(fit1), forecast(fit1, biasadj = TRUE)))
  expect_true(all.equal(forecast(fit2), forecast(fit2, biasadj = TRUE)))
  expect_true(all.equal(forecast(fit1)$mean, forecast(fit2)$mean))

  # ets
  fit <- ets(USAccDeaths, model = "ANA", lambda = 0.5, biasadj = TRUE)
  expect_true(all.equal(forecast(fit), forecast(fit, biasadj = TRUE)))

  # bats
  fit <- bats(USAccDeaths, use.box.cox = TRUE, biasadj = TRUE)
  expect_equal(forecast(fit), forecast(fit, biasadj = TRUE))

  # tbats
  fit <- tbats(USAccDeaths, use.box.cox = TRUE, biasadj = TRUE)
  expect_equal(forecast(fit), forecast(fit, biasadj = TRUE))
})

test_that("tests for automatic lambda selection in BoxCox transformation", {
  lambda_auto <- BoxCox.lambda(USAccDeaths)

  # lm
  fit <- tslm(USAccDeaths ~ trend, lambda = "auto", biasadj = TRUE)
  expect_equal(as.numeric(fit$lambda), lambda_auto, tolerance = 1e-3)

  # ets
  fit <- ets(USAccDeaths, model = "ANA", lambda = "auto", biasadj = TRUE)
  expect_equal(as.numeric(fit$lambda), lambda_auto, tolerance = 1e-3)

  # arima
  fit <- Arima(
    USAccDeaths,
    order = c(0, 1, 1),
    seasonal = c(0, 1, 1),
    lambda = "auto",
    biasadj = TRUE
  )
  expect_equal(as.numeric(fit$lambda), lambda_auto, tolerance = 1e-3)
})

test_that("InvBoxCox() adds no attributes to a single value", {
  lambda <- structure(0.5, biasadj = TRUE)
  x <- ts(5, start = 2000)
  expect_identical(InvBoxCox(x, 0.5), ts(12.25, start = 2000))
  expect_identical(InvBoxCox(x, lambda, TRUE, 1), ts(12.5, start = 2000))
  expect_identical(InvBoxCox(5, lambda, TRUE, 1), 12.5)
  expect_identical(InvBoxCox(5, structure(0.5, foo = 1)), 12.25)
  expect_identical(InvBoxCox(5, c(lam = 0.5)), 12.25)
})

test_that("InvBoxCox() keeps the names of a series with several values", {
  x <- ts(c(a = 1, b = 2))
  expect_named(InvBoxCox(x, 0), c("a", "b"))
})

test_that("forecasts with h = 1 and lambda have no stray attributes", {
  fc <- forecast(ets(Nile, "ANN", lambda = 0, biasadj = TRUE), h = 1)
  expect_named(attributes(fc$mean), c("tsp", "class"), ignore.order = TRUE)
  fc <- forecast(Arima(lynx, order = c(1, 0, 0), lambda = 0.5), h = 1)
  expect_named(attributes(fc$mean), c("tsp", "class"), ignore.order = TRUE)
  fc <- rwf(Nile, h = 1, lambda = 0, biasadj = TRUE)
  expect_named(attributes(fc$mean), c("tsp", "class"), ignore.order = TRUE)
})
