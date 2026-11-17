# dshw() checks the class of model

    Code
      dshw(taylor, model = ets(lh))
    Condition
      Error in `dshw()`:
      ! `model` must be the output of a previous call to `dshw()`.
    Code
      dshw(taylor, model = ses(lh))
    Condition
      Error in `dshw()`:
      ! `model` must be the output of a previous call to `dshw()`.
    Code
      dshw(taylor - mean(taylor), model = ets(lh))
    Condition
      Error in `dshw()`:
      ! `model` must be the output of a previous call to `dshw()`.

