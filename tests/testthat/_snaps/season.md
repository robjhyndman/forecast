# ma() checks order

    Code
      ma(1:20, order = 0)
    Condition
      Error in `ma()`:
      ! order must be a positive integer
    Code
      ma(1:20, order = 2.5)
    Condition
      Error in `ma()`:
      ! order must be a positive integer

