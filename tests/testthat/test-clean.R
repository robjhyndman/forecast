# A unit test for na.interp() and tsclean()
if (require(testthat)) {
  test_that("tests for na.interp", {
    # Test nonseasonal interpolation
    expect_true(all(na.interp(c(1, 2, 3, NA, 5, 6, 7)) == 1:7))
    # Test for identical on series without NAs
    expect_true(all(na.interp(wineind) == wineind))
    # Test seasonal interpolation
    testseries <- ts(rep(1:7, 5), frequency = 7)
    testseries[c(1, 3, 11, 17)] <- NA
    expect_true(sum(abs(na.interp(testseries) - rep(1:7, 5))) < 1e-14)
    # Test length of output
    expect_true(length(testseries) == length(na.interp(testseries)))
  })
  test_that("tests for tsclean", {
    # Test for no NAs
    expect_false(any(is.na(tsclean(gold))))
    # Test for removing outliers in seasonal series
    testseries <- ts(rep(1:7, 5), frequency = 7)
    testseries[c(2, 4, 14)] <- 0
    expect_true(sum(abs(tsclean(testseries) - rep(1:7, 5))) < 1e-14)
    # Test for NAs left with replace.missing = FALSE argument
    testseries[c(2, 4, 14)] <- NA
    expect_true(any(is.na(tsclean(testseries, replace.missing = FALSE))))
    # Test for outliers in a series
    expect_equal(sum(abs(wineind - tsclean(wineind)) > 1e-6), 1)
    # Test for identical on series without NAs or outliers
    expect_true(identical(USAccDeaths, tsclean(USAccDeaths)))
    # Test length of output
    expect_true(length(tsclean(testseries)) == length(testseries))
  })
}
