test_that("get_conf_level() returns sorted percentages", {
  expect_identical(get_conf_level(c(95, 80), fan = FALSE), c(80, 95))
  expect_equal(get_conf_level(c(0.95, 0.8), fan = FALSE), c(80, 95))
  expect_identical(get_conf_level(95, fan = TRUE), seq(51, 99, by = 3))
  expect_snapshot(get_conf_level(c(80, 110), fan = FALSE), error = TRUE)
})

test_that("normal_intervals() returns symmetric normal bounds", {
  hilo <- normal_intervals(ts(c(10, 20)), c(1, 2), c(80, 95))
  margin <- outer(c(1, 2), qnorm(c(0.9, 0.975)))
  expect_equal(unname(hilo$lower), c(10, 20) - margin)
  expect_equal(unname(hilo$upper), c(10, 20) + margin)
  expect_identical(colnames(hilo$lower), c("80%", "95%"))
  expect_identical(colnames(hilo$upper), c("80%", "95%"))

  hilo <- normal_intervals(c(10, 20), c(1, 2), 90)
  expect_identical(dim(hilo$lower), c(2L, 1L))
  expect_identical(colnames(hilo$lower), "90%")
})
