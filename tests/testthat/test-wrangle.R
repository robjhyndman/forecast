# A unit test for wrangling functions
if (require(testthat)) {
  context("Tests joining data.frames")

  mv_y <- ts(cbind(rnorm(120, 0, 3) + 1:120 + 20 * sin(2 * pi * (1:120) / 12), rnorm(120, 3, 7) + 1:120 + 16 * sin(2 * pi * (1:120 + 6) / 12)), frequency = 12)
  mv_x <- ts(cbind(rnorm(120, 0, 8) + (1:120) / 2 + 42 * sin(2 * pi * (1:120) / 12), rnorm(120, 3, 7) + (1:120) * -1 + 20 * sin(2 * pi * (1:120 + 6) / 12)), frequency = 12)
  v_y <- ts(rnorm(120, 0, 8) + (1:120) / 2 + 12 * sin(2 * pi * (1:120) / 12), frequency = 12)
  v_x <- ts(rnorm(120, 0, 1) + (1:120) * (-1) + 28 * sin(2 * pi * (1:120) / 12), frequency = 12)

  test_that("tests on retaining matrix attributes", {
    data <- datamat(mv_y, mv_x, v_y, v_x)
    expect_true(is.ts(data[, 1]))
    expect_true(identical(tsp(data[, 1]), tsp(data[, 2])))
    expect_true(NCOL(data) == 8)
    expect_true(NCOL(data[, 1]) == 2)
    expect_true("matrix" %in% class(data[, 1]))
    expect_true(class(data) == "data.frame")
  })

  test_that("flatten data.frames", {
    mvdata <- datamat(mv_y, mv_x)
    vdata <- datamat(v_y, v_x)
    data <- datamat(mvdata, vdata, flatten = TRUE)
    expect_true(class(data) == "data.frame")
    expect_true(!"data.frame" %in% class(data[, 1]))
  })
}
