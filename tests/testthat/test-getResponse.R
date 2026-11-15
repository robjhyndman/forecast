test_that("getResponse() does not partially match element names", {
  expect_null(getResponse(list(xtsp = c(1, 10, 1))))
  expect_null(getResponse(list(yhat = 1:3)))
  expect_identical(getResponse(list(x = 1:3, xreg = 4:6)), 1:3)
  expect_identical(getResponse(list(y = 1:3)), 1:3)
})

test_that("getResponse() returns the series for StructTS models", {
  fit <- StructTS(log10(UKgas), type = "level")
  expect_identical(getResponse(fit), log10(UKgas))
})
