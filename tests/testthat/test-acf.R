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
