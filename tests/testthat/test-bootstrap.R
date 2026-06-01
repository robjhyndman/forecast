test_that("bld.mbb.bootstrap returns the original series when num == 1", {
  res <- bld.mbb.bootstrap(WWWusage, 1)
  expect_length(res, 1)
  expect_equal(res[[1]], WWWusage)
})

test_that("bld.mbb.bootstrap keeps the original series first when num > 1", {
  res <- bld.mbb.bootstrap(WWWusage, 3)
  expect_length(res, 3)
  expect_equal(res[[1]], WWWusage)
})
