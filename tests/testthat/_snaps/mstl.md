# stlm() checks the class of model

    Code
      stlm(USAccDeaths, model = ets(USAccDeaths))
    Condition
      Error in `stlm()`:
      ! model should be the output of a previous call to stlm()
    Code
      stlm(lh, model = ets(lh))
    Condition
      Error in `stlm()`:
      ! model should be the output of a previous call to stlm()

