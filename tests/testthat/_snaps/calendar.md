# bizdays() requires monthly or quarterly data

    Code
      bizdays(ts(1:10, frequency = 7))
    Condition
      Error in `bizdays()`:
      ! `x` must be a monthly or quarterly time series.

---

    Code
      bizdays(1:20)
    Condition
      Error in `bizdays()`:
      ! `x` must be a monthly or quarterly time series.

---

    Code
      bizdays(x)
    Condition
      Error in `bizdays()`:
      ! `x` must be a monthly or quarterly time series.

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

---

    Code
      easter(x)
    Condition
      Error in `easter()`:
      ! `x` must be a monthly or quarterly time series.

