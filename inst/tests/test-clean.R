# A unit test for na.interp() and tsclean()
if(require(fpp) & require(testthat))
{
test_that("tests for na.interp", {
  expect_true(all(na.interp(c(1, 2, 3, NA, 5, 6, 7)) == 1:7))
  expect_true(all(na.interp(a10) == a10))
})
test_that("tests for tsclean",{
  expect_false(any(is.na(tsclean(gold))))
  myseries <- ts(rep(1:7, 4), f = 7)
  myseries[c(2, 4, 5, 14)] <- NA
  expect_true(all(round(tsclean(myseries), 10) == rep(1:7, 4)))

})
}
