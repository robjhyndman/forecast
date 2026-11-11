test_that("seasadj() removes the seasonal component", {
  fit <- stl(USAccDeaths, s.window = "periodic")
  expect_equal(seasadj(fit), USAccDeaths - seasonal(fit))

  fit <- decompose(USAccDeaths)
  expect_equal(seasadj(fit), USAccDeaths - seasonal(fit))

  fit <- decompose(AirPassengers, type = "multiplicative")
  expect_equal(seasadj(fit), AirPassengers / seasonal(fit))

  fit <- mstl(taylor)
  expect_equal(
    seasadj(fit),
    taylor - rowSums(seasonal(fit)),
    ignore_attr = TRUE
  )

  fit <- tbats(USAccDeaths, use.parallel = FALSE)
  expect_lt(var(seasadj(fit)), var(USAccDeaths))
  expect_equal(tsp(seasadj(fit)), tsp(USAccDeaths))
})

test_that("seasadj() matches the seasonal package", {
  skip_if_not_installed("seasonal")
  fit <- seasonal::seas(USAccDeaths)
  expect_equal(seasadj(fit), seasonal::final(fit))
})
