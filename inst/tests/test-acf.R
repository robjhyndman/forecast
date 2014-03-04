# A unit test for Acf() function
test_that("tests for acf", {
  out <- Acf(a10, lag.max = 10, type = "partial", plot = FALSE)
  attributes(out)
  out$acf
  expect_that(length(out$lag), equals(10))
  expect_that(out$acf, equals(Pacf(a10, lag.max = 10, plot = FALSE)$acf)
})
