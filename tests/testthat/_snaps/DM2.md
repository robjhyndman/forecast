# dm.test() errors for invalid input

    Code
      dm.test(e, rev(e), h = 0)
    Condition
      Error in `dm.test()`:
      ! h must be at least 1
    Code
      dm.test(e, rev(e), h = 10)
    Condition
      Error in `dm.test()`:
      ! h cannot be longer than the number of forecast errors
    Code
      dm.test(e, e)
    Condition
      Error in `dm.test()`:
      ! Variance of DM statistic is zero

