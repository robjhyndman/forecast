extract.ARIMA<-function (model, ...) 
{
  .local <- function (model, include.pvalues = FALSE, include.aic = TRUE, 
                      include.aicc = TRUE, include.bic = TRUE,
                      include.loglik = TRUE, ...) 
  {
    mask <- model$mask
    nam <- names(model$coef)
    co <- model$coef
    sdev <- sqrt(diag(model$var.coef))
    aicc <- model$aicc
    bic <- model$bic
    if (include.pvalues == TRUE) {
      t.rat <- rep(NA, length(mask))
      t.rat[mask] <- co[mask]/sdev
      pt <- 2 * pnorm(-abs(t.rat))
      setmp <- rep(NA, length(mask))
      setmp[mask] <- sdev
    }
    else {
      pt <- numeric()
      setmp <- sdev
    }
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.aic == TRUE) {
      aic <- AIC(model)
      gof <- c(gof, aic)
      gof.names <- c(gof.names, "AIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.aicc == TRUE) {
      gof <- c(gof, aicc)
      gof.names <- c(gof.names, "AICc")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.bic == TRUE) {
      gof <- c(gof, bic)
      gof.names <- c(gof.names, "BIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.loglik == TRUE) {
      lik <- model$loglik
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "Log Likelihood")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    tr <- createTexreg(coef.names = nam, coef = co, se = setmp, 
                       pvalues = pt, gof.names = gof.names, gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  .local(model, ...)
}

extract.ets<-function (model, ...) 
{
  .local <- function (model, include.pvalues = FALSE, include.aic = TRUE, 
                      include.aicc = TRUE, include.bic = TRUE,
                      include.loglik = TRUE, ...) 
  {
    mask <- model$mask
    nam <- names(model$par)
    co <- model$par
    sdev <- rep(-Inf,length(co))
    aicc <- model$aicc
    bic <- model$bic
    name=model$method
    if (include.pvalues == TRUE) {
      t.rat <- rep(NA, length(mask))
      t.rat[mask] <- co[mask]/sdev
      pt <- 2 * pnorm(-abs(t.rat))
      setmp <- rep(NA, length(mask))
      setmp[mask] <- sdev
    }
    else {
      pt <- numeric()
      setmp <- sdev
    }
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.aic == TRUE) {
      aic <- AIC(model)
      gof <- c(gof, aic)
      gof.names <- c(gof.names, "AIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.aicc == TRUE) {
      gof <- c(gof, aicc)
      gof.names <- c(gof.names, "AICc")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.bic == TRUE) {
      gof <- c(gof, bic)
      gof.names <- c(gof.names, "BIC")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.loglik == TRUE) {
      lik <- model$loglik
      gof <- c(gof, lik)
      gof.names <- c(gof.names, "Log Likelihood")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    tr <- createTexreg(coef.names = nam, coef = co, se = setmp, 
                       pvalues = pt, gof.names = gof.names, gof = gof, gof.decimal = gof.decimal,
                       model.name = name)
    return(tr)
  }
  .local(model, ...)
}

extract.forecast <- function (model, ...) 
{
  model <- model$model
  return(extract(model))
}

setMethod("extract", signature = className("ARIMA", "forecast"), definition = extract.ARIMA)
setMethod("extract", signature = className("ets", "forecast"), definition = extract.ets)
setMethod("extract", signature = className("forecast", "forecast"), definition = extract.forecast)