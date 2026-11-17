# tsCV errors when initial is negative

    Code
      tsCV(lynx, fnaive, h = 1, initial = -1)
    Condition
      Error in `tsCV()`:
      ! `initial` must be non-negative.

---

    Code
      tsCV(lynx, fnaive, h = 1, window = 5, initial = -10)
    Condition
      Error in `tsCV()`:
      ! `initial` must be non-negative.

