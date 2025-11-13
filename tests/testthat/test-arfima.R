# A unit test for arfima.R
arfima1 <- arfima(WWWusage, estim = "mle")
arfima2 <- arfima(WWWusage, estim = "ls")
arfimabc <- arfima(WWWusage, estim = "mle", lambda = 0.75, biasadj = FALSE)
arfimabc2 <- arfima(WWWusage, estim = "mle", lambda = 0.75, biasadj = TRUE)

test_that("test accuracy(), fitted(), and residuals().", {
  expect_identical(arimaorder(arfima1), arimaorder(arfima2))
  fitarfima <- fitted(arfima1)
  residarfima <- residuals(arfima2)
  expect_length(fitarfima, length(residarfima))
  expect_identical(getResponse(arfima1), WWWusage)
  expect_false(identical(arfimabc$fitted, arfimabc2$fitted))
  expect_no_error(accuracy(arfima1))
  expect_equal(mean(residuals(arfima1)), accuracy(arfima1)[, "ME"])
})

test_that("test forecast.fracdiff()", {
  expect_identical(
    forecast(arfima1, fan = TRUE)$mean,
    forecast(arfima1, fan = FALSE)$mean
  )
  expect_error(forecast(arfimabc, level = -10))
  expect_error(forecast(arfimabc, level = 110))
  expect_false(identical(
    forecast(arfimabc, biasadj = FALSE),
    forecast(arfimabc, biasadj = TRUE)
  ))
  expect_output(
    print(summary(forecast(arfimabc))),
    regexp = "Forecast method: ARFIMA"
  )
})
