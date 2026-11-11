test_that("as.character.tbats() formats the model string", {
  tbats_string <- function(...) {
    as.character(structure(list(...), class = "tbats"))
  }
  expect_snapshot({
    tbats_string()
    tbats_string(lambda = 0.12345, damping.parameter = 0.98765)
    tbats_string(ar.coefficients = c(0.1, 0.2), ma.coefficients = 0.3)
    tbats_string(seasonal.periods = 12, k.vector = 5)
    tbats_string(seasonal.periods = c(7, 365.256), k.vector = c(3, 10))
  })
})
