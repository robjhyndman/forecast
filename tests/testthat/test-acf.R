# A unit test for Acf() function
test_that("tests for acf", {
  out <- Acf(wineind, lag.max = 10, type = "partial", plot = FALSE)
  expect_length(out$lag, 10)
  expect_identical(out$acf, Pacf(wineind, lag.max = 10, plot = FALSE)$acf)
  expect_equal(
    dim(Acf(wineind, lag.max = 10, type = "correlation", plot = FALSE)$acf),
    c(11L, 1L, 1L)
  )
  expect_equal(
    Acf(wineind, lag.max = 10, type = "correlation", plot = TRUE)$acf[1, 1, 1],
    1
  )
})

test_that("acf works with msts objects", {
  x <- msts(rnorm(200), seasonal.periods = c(7, 365.25 / 7))
  out <- Acf(x, plot = FALSE)
  expect_s3_class(out, "acf")
})

test_that("taperedacf() and taperedpacf() return tapered estimates", {
  out <- taperedacf(wineind, lag.max = 10, plot = FALSE, calc.ci = FALSE)
  expect_s3_class(out, "mpacf")
  expect_length(out$z, 10)
  expect_null(out$upper)

  expect_identical(
    taperedpacf(wineind, lag.max = 10, plot = FALSE, calc.ci = FALSE)$z,
    taperedacf(
      wineind,
      lag.max = 10,
      type = "partial",
      plot = FALSE,
      calc.ci = FALSE
    )$z
  )

  set.seed(1)
  out <- taperedacf(wineind, lag.max = 10, plot = FALSE, nsim = 20)
  expect_length(out$upper, 10)
  expect_all_true(out$lower <= out$upper)
})
