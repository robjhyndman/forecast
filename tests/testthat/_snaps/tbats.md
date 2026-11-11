# tbats() errors when model is not a tbats or bats object

    Code
      tbats(USAccDeaths, model = ses(USAccDeaths))
    Condition
      Error in `tbats()`:
      ! model should be the output of a previous call to tbats() or bats()

