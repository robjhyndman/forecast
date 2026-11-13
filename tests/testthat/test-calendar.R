test_that("bizdays() differs across financial centers", {
  b1 <- bizdays(woolyrnq, FinCenter = "New York")
  b2 <- bizdays(woolyrnq, FinCenter = "London")
  b3 <- bizdays(woolyrnq, FinCenter = "Zurich")
  if (packageVersion("timeDate") >= "4021.105") {
    expect_equal(sum(abs(b1 - b2)), 145L)
    expect_equal(sum(abs(b1 - b3)), 176L)
  }
  expect_equal(sum(abs(b2 - b3)), 117L)
  b1 <- bizdays(gas, FinCenter = "NERC")
  b2 <- bizdays(gas, FinCenter = "Toronto")
  if (packageVersion("timeDate") >= "4021.105") {
    expect_equal(sum(abs(b1 - b2)), 211L)
  }
})

test_that("bizdays() counts trading days in each period", {
  x <- ts(1:3, start = c(2024, 1), frequency = 12)
  expect_identical(
    bizdays(x),
    ts(c(21L, 20L, 20L), start = c(2024, 1), frequency = 12)
  )
  x <- ts(1:2, start = c(2023, 4), frequency = 4)
  expect_identical(
    bizdays(x),
    ts(c(63L, 61L), start = c(2023, 4), frequency = 4)
  )
})

test_that("bizdays() requires monthly or quarterly data", {
  expect_snapshot(bizdays(ts(1:10, frequency = 7)), error = TRUE)
  expect_snapshot(bizdays(1:20), error = TRUE)
  x <- zoo::zooreg(1:3, start = zoo::as.yearmon(2020), frequency = 12)
  expect_snapshot(bizdays(x), error = TRUE)
})

test_that("easter() returns one value per period of x", {
  expect_length(easter(woolyrnq), length(woolyrnq))
  expect_length(easter(wineind), length(wineind))
})

test_that("easter() does not depend on where the series starts or ends", {
  full <- easter(ts(0, start = c(2014, 1), end = c(2019, 12), frequency = 12))
  expect_equal(
    as.numeric(window(full, start = c(2017, 3), end = c(2017, 4))),
    c(0, 1)
  )
  expect_equal(
    as.numeric(window(full, start = c(2018, 3), end = c(2018, 4))),
    c(0.67, 0.33)
  )
  part <- easter(ts(0, start = c(2014, 4), end = c(2018, 3), frequency = 12))
  expect_equal(part, window(full, start = c(2014, 4), end = c(2018, 3)))
  fullq <- easter(ts(0, start = c(2014, 1), end = c(2019, 4), frequency = 4))
  partq <- easter(ts(0, start = c(2014, 2), end = c(2018, 1), frequency = 4))
  expect_equal(partq, window(fullq, start = c(2014, 2), end = c(2018, 1)))
})

test_that("easter() includes Easter Monday when easter.mon = TRUE", {
  x <- ts(0, start = c(2013, 1), end = c(2018, 12), frequency = 12)
  mon <- easter(x, easter.mon = TRUE)
  expect_equal(
    as.numeric(window(easter(x), start = c(2013, 3), end = c(2013, 4))),
    c(1, 0)
  )
  expect_equal(
    as.numeric(window(mon, start = c(2013, 3), end = c(2013, 4))),
    c(0.75, 0.25)
  )
  expect_equal(
    as.numeric(window(mon, start = c(2017, 3), end = c(2017, 4))),
    c(0, 1)
  )
  expect_equal(
    as.numeric(window(mon, start = c(2018, 3), end = c(2018, 4))),
    c(0.5, 0.5)
  )
})

test_that("easter() returns fractions for quarterly data", {
  x <- ts(0, start = c(2017, 1), end = c(2018, 4), frequency = 4)
  expect_equal(as.numeric(easter(x)), c(0, 1, 0, 0, 0.67, 0.33, 0, 0))
  expect_equal(
    as.numeric(easter(x, easter.mon = TRUE)),
    c(0, 1, 0, 0, 0.5, 0.5, 0, 0)
  )
})

test_that("easter() requires monthly or quarterly data", {
  expect_snapshot(easter(ts(1:10, frequency = 7)), error = TRUE)
  expect_snapshot(easter(1:10), error = TRUE)
  x <- zoo::zooreg(1:3, start = zoo::as.yearmon(2020), frequency = 12)
  expect_snapshot(easter(x), error = TRUE)
})
