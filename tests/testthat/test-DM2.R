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
    dm.test(e, rev(e), h = 1.5)
    dm.test(e, rev(e), h = 10)
    dm.test(e, e)
  })
})

test_that("dm.test() ignores NA values in sample size and horizon checks", {
  set.seed(1)
  e1 <- rnorm(50)
  e2 <- rnorm(50, sd = 2)
  e1_na <- c(rep(NA_real_, 25), e1, NA_real_)
  e2_na <- c(rep(NA_real_, 25), e2, NA_real_)

  dm_clean <- dm.test(e1, e2, h = 1)
  dm_na <- dm.test(e1_na, e2_na, h = 1)
  expect_equal(dm_na$statistic, dm_clean$statistic)
  expect_equal(dm_na$p.value, dm_clean$p.value)

  dm_clean_h3 <- dm.test(e1, e2, h = 3, varestimator = "bartlett")
  dm_na_h3 <- dm.test(e1_na, e2_na, h = 3, varestimator = "bartlett")
  expect_equal(dm_na_h3$statistic, dm_clean_h3$statistic)
  expect_equal(dm_na_h3$p.value, dm_clean_h3$p.value)

  expect_error(
    dm.test(c(rep(NA_real_, 10), e1[1:3]), c(rep(NA_real_, 10), e2[1:3]), h = 4),
    "can't be longer than the number of forecast errors"
  )
})
