# tbats() errors when model is not a tbats or bats object

    Code
      tbats(USAccDeaths, model = ses(USAccDeaths))
    Condition
      Error in `tbats()`:
      ! model should be the output of a previous call to tbats() or bats()

# tbats() errors when refitting a model without parameters

    Code
      tbats(USAccDeaths, model = tbats(ts(rep(5, 30))))
    Condition
      Error in `tbats()`:
      ! model was fitted to a constant series and cannot be refitted
    Code
      tbats(USAccDeaths, model = structure(list(), class = c("tbats", "bats")))
    Condition
      Error in `tbats()`:
      ! model was fitted to a constant series and cannot be refitted

