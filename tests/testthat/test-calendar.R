# A unit test for calendar.R
if (require(testthat)) {
  test_that("Tests for bizdays()", {
    expect_error(bizdays(1:20))
    b1 <- bizdays(woolyrnq, FinCenter = "New York")
    b2 <- bizdays(woolyrnq, FinCenter = "London")
    b3 <- bizdays(woolyrnq, FinCenter = "Zurich")
    if(packageVersion("timeDate") >= '4021.105') {
        expect_equal(sum(abs(b1 - b2)), 145L)
        expect_equal(sum(abs(b1 - b3)), 176L)
    }
    expect_equal(sum(abs(b2 - b3)), 117L)
    b1 <- bizdays(gas, FinCenter = "NERC")
    b2 <- bizdays(gas, FinCenter = "Toronto")
    if(packageVersion("timeDate") >= '4021.105') {
        expect_equal(sum(abs(b1 - b2)), 211L)
    }
  })

  test_that("Tests for easter()", {
    expect_true(length(easter(woolyrnq)) == length(woolyrnq))
    expect_true(length(easter(wineind)) == length(wineind))
  })
}
