# A unit test for thetaf.R
if (require(testthat)) {
  test_that("test thetaf()", {
    thetafc <- thetaf(WWWusage)$mean
    expect_true(all(thetafc == thetaf(WWWusage, fan = TRUE)$mean))
    expect_error(thetaf(WWWusage, level = -10))
    expect_error(thetaf(WWWusage, level = 110))
    # Constant series should not error
    series <- ts(rep(950, 20), frequency = 4)
    constantForecast <- expect_error(thetaf(series), NA)
    expect_true(is.constant(round(constantForecast$mean, 12)))
  })
}
