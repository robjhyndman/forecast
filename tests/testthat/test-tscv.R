fnaive <- function(x, h, ...) forecast(naive(x, h = h, ...))

test_that("tsCV with h = 1 returns vector", {
  e <- tsCV(lynx, fnaive, h = 1)
  expect_s3_class(e, "ts")
  expect_null(dim(e))
  expect_length(e, length(lynx))
  expect_equal(tsp(e), tsp(lynx))
})

test_that("tsCV with h > 1 returns matrix", {
  e <- tsCV(lynx, fnaive, h = 3)
  expect_s3_class(e, "ts")
  expect_equal(ncol(e), 3)
  expect_equal(nrow(e), length(lynx))
  expect_equal(colnames(e), c("h=1", "h=2", "h=3"))
})

test_that("tsCV with rolling window", {
  e <- tsCV(lynx, fnaive, h = 1, window = 30)
  expect_length(e, length(lynx))
  expect_true(all(is.na(e[1:29])))
})

test_that("tsCV with initial", {
  e <- tsCV(lynx, fnaive, h = 1, initial = 10)
  expect_true(all(is.na(e[1:10])))
})

test_that("tsCV errors on bad input", {
  expect_error(
    tsCV(lynx, fnaive, h = 1, initial = length(lynx)),
    "initial period too long"
  )
})

test_that("tsCV with xreg", {
  n <- 50
  y <- ts(rnorm(n))
  xreg <- matrix(rnorm(n), ncol = 1)
  fxreg <- function(x, h, xreg, newxreg) {
    fit <- Arima(x, order = c(1, 0, 0), xreg = xreg)
    forecast(fit, h = h, xreg = newxreg)
  }
  e <- tsCV(y, fxreg, h = 1, xreg = xreg)
  expect_length(e, n)
})

test_that("tsCV errors when xreg wrong size", {
  y <- ts(rnorm(50))
  xreg <- matrix(rnorm(40), ncol = 1)
  expect_error(
    tsCV(y, fnaive, h = 1, xreg = xreg),
    "xreg must be of the same size as y"
  )
})

test_that("CVar basic usage", {
  set.seed(42)
  cv <- CVar(lynx, k = 3)
  expect_s3_class(cv, "CVar")
  expect_equal(cv$k, 3)
  expect_length(cv$testfit, length(lynx))
  expect_length(cv$residuals, length(lynx))
  expect_true(is.numeric(cv$LBpvalue))
  expect_equal(nrow(cv$CVsummary), 7)
  expect_equal(colnames(cv$CVsummary), c("Mean", "SD"))
})

test_that("CVar with blocked folds", {
  set.seed(42)
  cv <- CVar(lynx, k = 3, blocked = TRUE)
  expect_s3_class(cv, "CVar")
})

test_that("CVar errors with k < 2", {
  expect_error(CVar(lynx, k = 1), "k must be at least 2")
})

test_that("CVar k is capped at series length", {
  y <- ts(1:5)
  set.seed(42)
  cv <- CVar(y, k = 100)
  expect_equal(cv$k, 5)
})

test_that("print.CVar", {
  set.seed(42)
  cv <- CVar(lynx, k = 3)
  expect_output(print(cv), "cross-validation")
})
