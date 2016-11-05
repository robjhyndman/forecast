# A unit test for h-step fits
if(require(testthat))
{
  context("Tests for h-step fits with hfitted")

  test_that("variance test on h-step fits", {
    mod1 <- ets(WWWusage)
    h1 <- fitted(mod1, h=1)
    h2 <- fitted(mod1, h=2)
    h3 <- fitted(mod1, h=3)
    j <- !is.na(h1) & !is.na(h2) & !is.na(h3)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))
    expect_lt(var(diff(h2[j])), var(diff(h3[j])))

    mod2 <- auto.arima(WWWusage)
    h1 <- fitted(mod2, h=1)
    h2 <- fitted(mod2, h=2)
    h3 <- fitted(mod2, h=3)
    j <- !is.na(h1) & !is.na(h2) & !is.na(h3)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))
    expect_lt(var(diff(h2[j])), var(diff(h3[j])))

    mod3 <- tbats(WWWusage)
    h1 <- fitted(mod3, h=1)
    h2 <- fitted(mod3, h=2)
    h3 <- fitted(mod3, h=3)
    j <- !is.na(h1) & !is.na(h2) & !is.na(h3)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))
    expect_lt(var(diff(h2[j])), var(diff(h3[j])))

    mod4 <- bats(WWWusage)
    h1 <- fitted(mod4, h=1)
    h2 <- fitted(mod4, h=2)
    h3 <- fitted(mod4, h=3)
    j <- !is.na(h1) & !is.na(h2) & !is.na(h3)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))
    expect_lt(var(diff(h2[j])), var(diff(h3[j])))

    mod5 <- nnetar(WWWusage)
    h1 <- fitted(mod5, h=1)
    h2 <- fitted(mod5, h=2)
    h3 <- fitted(mod5, h=3)
    j <- !is.na(h1) & !is.na(h2) & !is.na(h3)
    expect_lt(var(diff(h1[j])), var(diff(h2[j])))
    expect_lt(var(diff(h2[j])), var(diff(h3[j])))
  })
}
