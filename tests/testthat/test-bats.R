# A unit test for bats function
test_that("tests for a non-ts object", {
  set.seed(123)
  abc <- rnorm(50, 5, 1)
  fit <- bats(abc, use.box.cox = TRUE, use.parallel = FALSE)
  expect_false(fit$lambda == 0)
  expect_output(print(fit), "Seed States")
  expect_length(residuals(fit), 50L)
  plot(fit)
  expect_equal(bats(1, use.box.cox = TRUE, use.parallel = FALSE)$AIC, -Inf)
  expect_equal(bats(-1, use.box.cox = TRUE, use.parallel = FALSE)$AIC, -Inf)
})

test_that("Test parallel of bats", {
  abc <- rnorm(50, 5, 1)
  skip_on_cran()
  skip_on_ci()
  expect_gt(bats(abc, use.box.cox = TRUE, use.parallel = TRUE)$lambda, 0.999)
})
