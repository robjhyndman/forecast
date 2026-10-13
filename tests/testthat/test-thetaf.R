# A unit test for thetaf.R
test_that("test thetaf()", {
  thetafc <- thetaf(WWWusage)$mean
  expect_true(all(thetafc == thetaf(WWWusage, fan = TRUE)$mean))
  expect_error(thetaf(WWWusage, level = -10))
  expect_error(thetaf(WWWusage, level = 110))
  # Constant series should not error
  series <- ts(rep(950, 20), frequency = 4)
  constantForecast <- expect_no_error(thetaf(series))
  expect_true(is.constant(round(constantForecast$mean, 12)))
})

test_that("thetaf() works with deprecated x argument", {
  fc <- thetaf(x = USAccDeaths)
  expect_identical(fc$series, "USAccDeaths")
  expect_length(fc$mean, 24L)
})
