# A unit test for tbats function
test_that("Test simple cases for tbats", {
  expect_error(tbats(data.frame(x1 = 1, x2 = 2), use.parallel = FALSE))
  expect_warning(tbats(c(1:5, NA, 7:9), use.parallel = FALSE))
  expect_true(all(forecast(tbats(rep(1, 100), use.parallel = FALSE))$mean == 1))
})

test_that("Test tbats() and forecasts", {
  # Fit tbats models
  tbatsfit1 <- tbats(subset(wineind, end = 50), use.parallel = FALSE)
  tbatsfit2 <- tbats(WWWusage, use.parallel = FALSE)
  tbatsfit3 <- tbats(
    as.numeric(woolyrnq),
    seasonal.periods = frequency(woolyrnq),
    use.parallel = FALSE
  )
  tbatsfit4 <- tbats(airmiles, use.box.cox = FALSE, use.parallel = FALSE)
  # Test tbats.components
  tbats.components(tbatsfit1)
  tbats.components(tbatsfit2)
  tbats.components(tbatsfit3)
  tbats.components(tbatsfit4)
  # Test accuracy.tbats() function
  expect_output(print(accuracy(tbatsfit1)), regexp = "ME")
  expect_output(print(accuracy(tbatsfit2)), regexp = "ME")
  expect_output(print(accuracy(tbatsfit3)), regexp = "ME")
  expect_output(print(accuracy(tbatsfit4)), regexp = "ME")
  # Test summary.tbats()
  expect_output(print(summary(tbatsfit1)), regexp = "Length")
  expect_output(print(summary(tbatsfit2)), regexp = "Length")
  expect_output(print(summary(tbatsfit3)), regexp = "Length")
  expect_output(print(summary(tbatsfit4)), regexp = "Length")
  # Test fitted length
  expect_length(fitted(tbatsfit1), 50)
  expect_length(fitted(tbatsfit2), length(WWWusage))
  expect_length(fitted(tbatsfit3), length(woolyrnq))
  expect_length(fitted(tbatsfit4), length(airmiles))
  # Test length of forecast
  expect_length(forecast(tbatsfit1)$mean, 2 * frequency(wineind))
  expect_length(forecast(tbatsfit2)$mean, 10)
  expect_length(forecast(tbatsfit4)$mean, 10)
  # Test inappropriate levels
  expect_error(forecast(tbatsfit1, level = -10))
  expect_error(forecast(tbatsfit1, level = 110))
  # Test forecasts with fan = TRUE
  expect_true(all(
    forecast(tbatsfit1, fan = TRUE)$mean == forecast(tbatsfit1)$mean
  ))
})

test_that("tbats() selects harmonics independently of period order", {
  set.seed(42)
  y <- 10 +
    sin(2 * pi * (1:120) / 4) +
    0.5 * sin(2 * pi * (1:120) / 6) +
    rnorm(120, sd = 0.1)
  fit <- function(periods) {
    tbats(
      y,
      seasonal.periods = periods,
      use.box.cox = FALSE,
      use.trend = FALSE,
      use.arma.errors = FALSE,
      use.parallel = FALSE
    )
  }
  fits <- lapply(list(c(6, 4), c(4, 6)), fit)
  fields <- c("seasonal.periods", "k.vector", "AIC")

  expect_equal(fits[[1]][fields], fits[[2]][fields])
  expect_equal(fits[[1]]$k.vector, c(1, 1))
})

test_that("tbats() errors when model is not a tbats or bats object", {
  expect_snapshot(error = TRUE, {
    tbats(USAccDeaths, model = ses(USAccDeaths))
  })
})

test_that("tbats() works when only use.damped.trend is set", {
  fit <- tbats(UKgas, use.damped.trend = TRUE, use.parallel = FALSE)
  expect_s3_class(fit, "tbats")
})

test_that("tbats() searches below and above six harmonics for long periods", {
  fit <- function(y) {
    tbats(
      y,
      use.box.cox = FALSE,
      use.trend = FALSE,
      use.arma.errors = FALSE,
      use.parallel = FALSE
    )
  }
  set.seed(42)
  idx <- 1:360
  y <- 100 + 10 * sin(2 * pi * idx / 24) + 4 * cos(2 * pi * 3 * idx / 24)
  down <- fit(ts(y + rnorm(360, sd = 0.5), frequency = 24))
  expect_gte(down$k.vector, 3)
  expect_lt(down$k.vector, 6)

  idx <- 1:260
  y <- 10 + sin(2 * pi * idx / 52) + 0.5 * sin(2 * pi * 8 * idx / 52)
  up <- fit(ts(y + rnorm(260, sd = 0.1), frequency = 52))
  expect_gte(up$k.vector, 8)
})

test_that("tbats() selects the same model in parallel", {
  skip_on_cran()
  skip_on_ci()
  set.seed(42)
  idx <- 1:360
  y <- 100 + 10 * sin(2 * pi * idx / 24) + 4 * cos(2 * pi * 3 * idx / 24)
  y <- ts(y + rnorm(360), frequency = 24)
  fields <- c("k.vector", "AIC")
  expect_equal(
    tbats(y, use.arma.errors = FALSE, use.parallel = TRUE, num.cores = 2)[
      fields
    ],
    tbats(y, use.arma.errors = FALSE, use.parallel = FALSE)[fields]
  )
})
