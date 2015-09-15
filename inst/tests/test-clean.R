# A unit test for na.interp() and tsclean()
if(require(fpp) & require(testthat))
{
test_that("tests for na.interp", {
  # Test nonseasonal interpolation
  expect_true(all(na.interp(c(1, 2, 3, NA, 5, 6, 7)) == 1:7))
  # Test for identical on series without NAs
  expect_true(all(na.interp(a10) == a10))
  # Test seasonal interpolation
  testseries <- ts(rep(1:7, 5), f = 7)
  testseries[c(1, 3, 11, 17)] <- NA
  all(na.interp(testseries) == rep(1:7, 5))
  # Test length of output
  expect_true(length(testseries) == length(na.interp(testseries)))
})
test_that("tests for tsclean",{
  # Test for no NAs
  expect_false(any(is.na(tsclean(gold))))
  # Test for removing outliers in seasonal series
  testseries <- ts(rep(1:7, 5), f = 7)
  testseries[c(2, 4, 14)] <- 0
  expect_true(all(tsclean(testseries) == rep(1:7, 5)))
  # Test for NAs left with replace.missing = FALSE argument
  testseries[c(2, 4, 14)] <- NA
  expect_true(any(is.na(tsclean(testseries, replace.missing = FALSE))))
  # Test for identical on series without NAs or outliers
  expect_true(all(wineind == tsclean(wineind)))
  # Test length of output
  expect_true(length(tsclean(testseries)) == length(testseries))
})
}
