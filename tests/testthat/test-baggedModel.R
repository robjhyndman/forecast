test_that("forecast.baggedModel returns interval matrices", {
  set.seed(1)
  fit <- baggedETS(WWWusage, bld.mbb.bootstrap(WWWusage, 3))
  fc <- forecast(fit, h = 4)
  expect_identical(dim(fc$lower), c(4L, 1L))
  expect_identical(colnames(fc$upper), "100%")
  expect_identical(dim(forecast(fc, h = 2)$upper), c(2L, 1L))
})

test_that("forecast.forecast() shortens the median and bootstrap forecasts", {
  set.seed(1)
  fit <- baggedETS(WWWusage, bld.mbb.bootstrap(WWWusage, 3))
  fc <- forecast(forecast(fit, h = 4), h = 2)
  expect_identical(tsp(fc$median), tsp(fc$mean))
  expect_identical(dim(fc$forecasts_boot), c(2L, 3L))
  expect_identical(tsp(fc$forecasts_boot), tsp(fc$mean))
})
