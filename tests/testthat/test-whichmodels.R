test_that("WhichModels produces correct model strings", {
  expect_identical(WhichModels(0, 0, 0, 0, 0), "0f0f0f0f0")
  expect_length(WhichModels(2, 1, 1, 1, 1), 3 * 2 * 2 * 2 * 2)
  expect_identical(
    WhichModels(1, 1, 0, 0, 0),
    c("0f0f0f0f0", "0f1f0f0f0", "1f0f0f0f0", "1f1f0f0f0")
  )
})

test_that("WhichModels matches original loop order", {
  actual <- WhichModels(2, 1, 3, 1, 4)
  expected <- character()
  for (x1 in 0:2) {
    for (x2 in 0:1) {
      for (x3 in 0:3) {
        for (x4 in 0:1) {
          for (K in 0:4) {
            expected <- c(
              expected,
              paste0(x1, "f", x2, "f", x3, "f", x4, "f", K)
            )
          }
        }
      }
    }
  }
  expect_identical(actual, expected)
})
