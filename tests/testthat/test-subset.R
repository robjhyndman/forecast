# A unit test for subset function
mtsobj <- ts(matrix(rnorm(200), ncol = 2), frequency = 4)
test_that("tests specifying correct argument", {
  sub <- subset(wineind, month = "September")
  expect_length(sub, tsp(sub)[2] - tsp(sub)[1] + 1)
  expect_identical(round(sum(sub)), 338985)
  sub2 <- subset(wineind, month = "SEPT")
  expect_identical(sub, sub2)
  sub2 <- subset(wineind, month = 9)
  expect_identical(sub, sub2)
  sub2 <- subset(wineind, season = 9)
  expect_identical(sub, sub2)
  sub <- subset(woolyrnq, quarter = 1)
  expect_length(sub, tsp(sub)[2] - tsp(sub)[1] + 1)
  expect_identical(sum(sub), 153142)
  sub2 <- subset(woolyrnq, season = 1)
  expect_identical(sub, sub2)
  sub <- subset(wineind, subset = wineind < 25000)
  expect_identical(round(sum(sub)), 1948985)
  expect_length(sub, 91)
  sub <- subset(mtsobj, c(1, 1, rep(0, 98)) == 1)
  expect_shape(sub, ncol = 2L)
  expect_shape(sub, nrow = 2L)
  sub <- subset(mtsobj, quarter = 1)
  expect_shape(sub, ncol = 2L)
  expect_shape(sub, nrow = 25L)
})

test_that("tests specifying wrong argument", {
  expect_error(subset(wineind, quarter = 1), "Data is not quarterly")
  expect_error(subset(woolyrnq, month = "January"), "Data is not monthly")
})

test_that("test for bad input", {
  expect_error(subset.ts(mtcars, quarter = 1), "Data must be seasonal")
  expect_error(
    subset(wineind, subset = c(1, 2)),
    "subset must be the same length as x"
  )
  expect_error(
    subset(mtsobj, mtsobj < .5),
    "subset must be a vector of rows to keep"
  )
  expect_error(subset(wineind, month = "Jaan"), "No recognizable months")
  expect_error(
    subset(wineind, season = 1:14),
    "Seasons must be between 1 and 12"
  )
  expect_error(subset(wineind, month = 1:14), "Months must be between 1 and 12")
  expect_error(subset(woolyrnq, quarter = "qq1"), "No recognizable quarters")
  expect_error(
    subset(woolyrnq, quarter = 1:6),
    "Quarters must be between 1 and 4"
  )
})
