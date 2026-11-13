# A unit test for spline.R
test_that("Tests for splinef()", {
  fit <- spline_model(airmiles)
  expect_equal(fit$beta, 0.5591, tolerance = 1e-4)
  fc <- forecast(fit)
  expect_true(all(as.data.frame(fc) > 24000))
  expect_no_error(autoplot(fc))
  fit1 <- splinef(woolyrnq, lambda = 0.2, biasadj = FALSE)
  fit2 <- splinef(woolyrnq, lambda = 0.2, biasadj = TRUE)
  expect_false(identical(fit1$mean, fit2$mean))
  splinef(woolyrnq, method = "mle")
  splinef(WWWusage, method = "mle")
  expect_error(splinef(woolyrnq, level = 110))
  expect_error(splinef(woolyrnq, level = -10))
})

test_that("splinef() returns univariate time series", {
  expect_null(dim(splinef(uspop, h = 3)$mean))
  expect_null(dim(splinef(uspop, h = 3, lambda = 0)$mean))
  expect_s3_class(splinef(uspop, method = "mle")$fitted, "ts")
})

test_that("splinef() returns lambda as a component", {
  fc <- splinef(uspop, h = 3, lambda = 0)
  expect_equal(fc$lambda, 0, ignore_attr = TRUE)
  expect_null(attr(fc, "lambda"))
})

test_that("splinef() keeps series name", {
  expect_identical(splinef(airmiles)$series, "airmiles")
  expect_identical(splinef(x = airmiles)$series, "airmiles")
})
