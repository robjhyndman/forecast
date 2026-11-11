test_that("as.character.bats() formats the model string", {
  bats_string <- function(...) {
    as.character(structure(list(...), class = "bats"))
  }
  expect_snapshot({
    bats_string()
    bats_string(lambda = 0.12345, damping.parameter = 0.98765)
    bats_string(ar.coefficients = c(0.1, 0.2), ma.coefficients = 0.3)
    bats_string(seasonal.periods = 12)
    bats_string(seasonal.periods = c(7, 365.25))
  })
})
