test_that("checkresiduals test argument works", {
  fit <- ets(WWWusage)
  lb <- checkresiduals(fit, test = "LB", plot = FALSE)
  expect_s3_class(lb, "htest")
  expect_invisible(checkresiduals(fit, test = FALSE, plot = FALSE))
  lb_lag <- checkresiduals(fit, lag = 5, plot = FALSE)
  expect_equal(lb_lag$parameter, c(df = 5))
})

test_that("checkresiduals works with ets model", {
  fit <- ets(WWWusage)
  lb <- checkresiduals(fit, plot = FALSE)
  expect_s3_class(lb, "htest")
})

test_that("checkresiduals works with Arima model", {
  fit <- Arima(WWWusage, order = c(1, 1, 0))
  lb <- checkresiduals(fit, plot = FALSE)
  expect_s3_class(lb, "htest")
  expect_equal(lb$parameter, c(df = 10 - 1))
})

test_that("checkresiduals works with lm model", {
  lm_fit <- lm(y ~ x, data = data.frame(y = USAccDeaths, x = time(USAccDeaths)))
  bg <- checkresiduals(lm_fit, plot = FALSE)
  expect_s3_class(bg, "htest")
})

test_that("checkresiduals works with bats model", {
  fit <- bats(WWWusage)
  lb <- checkresiduals(fit, plot = FALSE)
  expect_s3_class(lb, "htest")
})

test_that("checkresiduals works with rw_model", {
  fit <- rw_model(gold)
  lb <- checkresiduals(fit, plot = FALSE)
  expect_s3_class(lb, "htest")
})

test_that("checkresiduals works with raw time series", {
  expect_output(checkresiduals(residuals(ets(WWWusage)), plot = FALSE))
})

test_that("checkresiduals works with forecast object", {
  fc <- forecast(ets(WWWusage))
  expect_output(checkresiduals(fc, plot = FALSE))
})
