# bats() errors when model is not a bats object

    Code
      bats(USAccDeaths, model = ses(USAccDeaths))
    Condition
      Error in `bats()`:
      ! model should be the output of a previous call to bats()
    Code
      bats(USAccDeaths, model = structure(list(), class = c("tbats", "bats")))
    Condition
      Error in `bats()`:
      ! model should be the output of a previous call to bats()

# bats() errors when refitting a constant model

    Code
      bats(USAccDeaths, model = bats(ts(rep(5, 30))))
    Condition
      Error in `bats()`:
      ! model was fitted to a constant series and cannot be refitted

