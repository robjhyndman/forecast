test_that("dm.test() with h = 1 matches a t-test on the loss differential", {
  set.seed(1)
  e1 <- rnorm(50)
  e2 <- rnorm(50, sd = 2)

  dm <- dm.test(e1, e2)
  tt <- t.test(e1^2 - e2^2)
  expect_s3_class(dm, "htest")
  expect_equal(dm$statistic, tt$statistic, ignore_attr = TRUE)
  expect_equal(dm$p.value, tt$p.value, ignore_attr = TRUE)

  expect_equal(
    dm.test(e1, e2, power = 1)$p.value,
    t.test(abs(e1) - abs(e2))$p.value,
    ignore_attr = TRUE
  )
  expect_identical(
    dm.test(e1, e2, varestimator = "bartlett")$statistic,
    dm$statistic
  )
})

test_that("dm.test() uses Bartlett weights for h > 1", {
  set.seed(1)
  e1 <- rnorm(50)
  e2 <- rnorm(50, sd = 2)
  n <- 50
  h <- 3

  d <- e1^2 - e2^2
  autocov <- acf(d, lag.max = h - 1, type = "covariance", plot = FALSE)$acf
  weights <- c(1, 2 * (1 - seq_len(h - 1) / h))
  d_var <- sum(weights * autocov) / n
  k <- sqrt((n + 1 - 2 * h + (h / n) * (h - 1)) / n)

  expect_equal(
    dm.test(e1, e2, h = h, varestimator = "bartlett")$statistic,
    mean(d) / sqrt(d_var) * k,
    ignore_attr = TRUE
  )
})

test_that("dm.test() alternatives are consistent", {
  set.seed(1)
  e1 <- rnorm(50)
  e2 <- rnorm(50, sd = 2)

  expect_no_warning({
    two <- dm.test(e1, e2, h = 3)
    less <- dm.test(e1, e2, alternative = "less", h = 3)
    greater <- dm.test(e1, e2, alternative = "greater", h = 3)
  })
  expect_equal(less$p.value + greater$p.value, 1, ignore_attr = TRUE)
  expect_equal(
    two$p.value,
    2 * min(less$p.value, greater$p.value),
    ignore_attr = TRUE
  )
  expect_equal(dm.test(e2, e1, h = 3)$statistic, -two$statistic)
})

test_that("dm.test() errors for invalid input", {
  e <- c(1, -2, 3, -1, 2)
  expect_snapshot(error = TRUE, {
    dm.test(e, rev(e), h = 0)
    dm.test(e, rev(e), h = 10)
    dm.test(e, e)
  })
})
