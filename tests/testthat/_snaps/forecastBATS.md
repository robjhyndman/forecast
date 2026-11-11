# as.character.bats() formats the model string

    Code
      bats_string()
    Output
      [1] "BATS(1, {0,0}, -, -)"
    Code
      bats_string(lambda = 0.12345, damping.parameter = 0.98765)
    Output
      [1] "BATS(0.123, {0,0}, 0.988, -)"
    Code
      bats_string(ar.coefficients = c(0.1, 0.2), ma.coefficients = 0.3)
    Output
      [1] "BATS(1, {2,1}, -, -)"
    Code
      bats_string(seasonal.periods = 12)
    Output
      [1] "BATS(1, {0,0}, -, {12})"
    Code
      bats_string(seasonal.periods = c(7, 365.25))
    Output
      [1] "BATS(1, {0,0}, -, {7,365.25})"

