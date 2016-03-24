Sys.setenv("R_TESTS" = "")
if(require(testthat) & require(fpp))
  test_check("forecast")
