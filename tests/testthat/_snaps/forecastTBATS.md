# as.character.tbats() formats the model string

    Code
      tbats_string()
    Output
      [1] "TBATS(1, {0,0}, -,{-})"
    Code
      tbats_string(lambda = 0.12345, damping.parameter = 0.98765)
    Output
      [1] "TBATS(0.123, {0,0}, 0.988,{-})"
    Code
      tbats_string(ar.coefficients = c(0.1, 0.2), ma.coefficients = 0.3)
    Output
      [1] "TBATS(1, {2,1}, -,{-})"
    Code
      tbats_string(seasonal.periods = 12, k.vector = 5)
    Output
      [1] "TBATS(1, {0,0}, -, {<12,5>})"
    Code
      tbats_string(seasonal.periods = c(7, 365.256), k.vector = c(3, 10))
    Output
      [1] "TBATS(1, {0,0}, -, {<7,3>, <365.26,10>})"

