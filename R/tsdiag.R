#' @export
tsdiag.ets <- function (object, gof.lag = 10, ...) 
{
    oldpar <- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    rs <- object$residuals
    stdres <- rs/sqrt(object$sigma2)
    plot(stdres, type = "h", main = "Standardized Residuals", 
        ylab = "")
    abline(h = 0)
    Acf(object$residuals, plot = TRUE, main = "ACF of Residuals", 
        na.action = na.pass)
    nlag <- gof.lag
    pval <- numeric(nlag)
    for (i in 1:nlag) pval[i] <- Box.test(rs, i, type = "Ljung-Box")$p.value
    plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0, 
        1), main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}
