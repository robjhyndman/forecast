test_that("non-numeric alpha errors", {
  expect_snapshot(ses(WWWusage, initial = "simple", alpha = TRUE), error = TRUE)
})
