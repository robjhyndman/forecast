test_that("stl components add up to the data", {
  fit <- stl(USAccDeaths, s.window = "periodic")
  expect_equal(seasonal(fit) + trendcycle(fit) + remainder(fit), USAccDeaths)
})

test_that("component extractors work for decomposed.ts", {
  fit <- decompose(USAccDeaths)
  expect_identical(seasonal(fit), fit$seasonal)
  expect_identical(trendcycle(fit), fit$trend)
  expect_identical(remainder(fit), fit$random)
})

test_that("mstl components add up to the data", {
  fit <- mstl(taylor)
  expect_identical(colnames(seasonal(fit)), c("Seasonal48", "Seasonal336"))
  expect_equal(
    trendcycle(fit) + rowSums(seasonal(fit)) + remainder(fit),
    taylor,
    ignore_attr = TRUE
  )
})

test_that("seasonal() works for tbats", {
  fit <- tbats(USAccDeaths, use.parallel = FALSE)
  expect_equal(tsp(seasonal(fit)), tsp(USAccDeaths))
})

test_that("component extractors match the seasonal package", {
  skip_if_not_installed("seasonal")
  fit <- seasonal::seas(USAccDeaths)
  expect_equal(trendcycle(fit), seasonal::trend(fit))
  expect_equal(remainder(fit), seasonal::irregular(fit))
})

test_that("component extractors error for unknown objects", {
  expect_snapshot(error = TRUE, {
    seasonal(1)
    trendcycle(1)
    remainder(1)
  })
})
