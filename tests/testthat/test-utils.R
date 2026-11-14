test_that("get_conf_level() returns sorted percentages", {
  expect_identical(get_conf_level(c(95, 80), fan = FALSE), c(80, 95))
  expect_equal(get_conf_level(c(0.95, 0.8), fan = FALSE), c(80, 95))
  expect_identical(get_conf_level(95, fan = TRUE), seq(51, 99, by = 3))
  expect_snapshot(get_conf_level(c(80, 110), fan = FALSE), error = TRUE)
})
