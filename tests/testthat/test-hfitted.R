# A unit test for h-step fits
if (require(testthat)) {
  context("Tests for h-step fits with hfitted")

  test_that("variance test on h-step fits", {
    mod1 <- ets(WWWusage, model = "AAN", damped = TRUE)
    h1 <- fitted(mod1, h = 1)
    h2 <- fitted(mod1, h = 2)
    j <- !is.na(h1) & !is.na(h2)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))
    # hfitted automatic function selection
    h2_1 <- hfitted(mod1, h = 2)
    expect_true(identical(h2, h2_1))

    mod2 <- Arima(WWWusage, order = c(1, 1, 1))
    h1 <- fitted(mod2, h = 1)
    h2 <- fitted(mod2, h = 2)
    j <- !is.na(h1) & !is.na(h2)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))

    mod3 <- arfima(WWWusage)
    h1 <- fitted(mod3, h = 1)
    h2 <- fitted(mod3, h = 2)
    j <- !is.na(h1) & !is.na(h2)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))

    # mod3 <- tbats(WWWusage)
    # h1 <- fitted(mod3, h=1)
    # h2 <- fitted(mod3, h=2)
    # j <- !is.na(h1) & !is.na(h2)
    # expect_lt(var(diff(h1[j])), var(diff(h2[j])))
    #
    # mod4 <- bats(WWWusage)
    # h1 <- fitted(mod4, h=1)
    # h2 <- fitted(mod4, h=2)
    # j <- !is.na(h1) & !is.na(h2)
    # expect_lt(var(diff(h1[j])), var(diff(h2[j])))

    mod5 <- nnetar(WWWusage)
    h1 <- fitted(mod5, h = 1)
    h2 <- fitted(mod5, h = 2)
    j <- !is.na(h1) & !is.na(h2)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))
  })
}
