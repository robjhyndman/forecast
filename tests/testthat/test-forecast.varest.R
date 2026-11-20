test_that("forecast.varest() works for a VAR fitted to a data frame", {
  skip_if_not_installed("vars")
  y <- vars::Canada
  fc_ts <- forecast(vars::VAR(y, p = 2), h = 4)
  fc_df <- forecast(vars::VAR(as.data.frame(y), p = 2), h = 4)
  expect_equal(fc_df$forecast$e$mean, fc_ts$forecast$e$mean, ignore_attr = TRUE)
  expect_equal(tsp(fc_df$forecast$e$mean), c(nrow(y) + 1, nrow(y) + 4, 1))
  expect_equal(tsp(fc_df$forecast$e$x), c(1, nrow(y), 1))
})
