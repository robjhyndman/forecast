test_that("ndiffs with trend type", {
  expect_true(ndiffs(AirPassengers, type = "trend") >= 0)
})

test_that("ndiffs with max.d", {
  d <- ndiffs(WWWusage, max.d = 1)
  expect_true(d <= 1)
})

test_that("ndiffs returns 0 for constant series", {
  expect_equal(ndiffs(ts(rep(5, 100))), 0)
})

test_that("ndiffs warns on extreme alpha", {
  expect_warning(ndiffs(AirPassengers, alpha = 0.001), "less than the minimum")
  expect_warning(ndiffs(AirPassengers, alpha = 0.5), "larger than the maximum")
})

test_that("nsdiffs warns on extreme alpha", {
  expect_warning(nsdiffs(AirPassengers, alpha = 0.001), "less than the minimum")
  expect_warning(nsdiffs(AirPassengers, alpha = 0.5), "larger than the maximum")
})

test_that("nsdiffs returns 0 for constant series", {
  expect_equal(nsdiffs(ts(rep(5, 100), frequency = 12)), 0)
})

test_that("nsdiffs returns 0 for series shorter than frequency", {
  x <- ts(1:5, frequency = 12)
  expect_equal(nsdiffs(x), 0)
})

test_that("ocsb.test returns OCSBtest object", {
  result <- ocsb.test(AirPassengers)
  expect_s3_class(result, "OCSBtest")
  expect_output(print(result), "OCSB test")
})

test_that("ocsb.test errors on non-seasonal data", {
  expect_error(ocsb.test(Nile), "seasonal")
})

test_that("ocsb.test with lag selection", {
  result <- ocsb.test(AirPassengers, lag.method = "AIC", maxlag = 2)
  expect_s3_class(result, "OCSBtest")
})
