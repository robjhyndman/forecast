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
  # expect_true(length(forecast(tbatsfit3)$mean) == 2 * frequency(woolyrnq))
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

#test_that("Test tbats() with parallel", {
# Tests will not run on Travis in parallel
# expect_output(print(tbats(woolyrnq, num.cores = 1)), regexp = "TBATS")
# expect_output(print(tbats(elecsales, num.cores = 1, use.trend = FALSE)), regexp = "BATS")
#})
