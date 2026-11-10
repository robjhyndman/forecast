# easter() requires monthly or quarterly data

    Code
      easter(ts(1:10, frequency = 7))
    Condition
      Error in `easter()`:
      ! `x` must be a monthly or quarterly time series.

---

    Code
      easter(1:10)
    Condition
      Error in `easter()`:
      ! `x` must be a monthly or quarterly time series.

