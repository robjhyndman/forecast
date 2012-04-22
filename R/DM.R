#Diebold-Mariano test. Modified from code by Adrian Trapletti.

dm.test <- function(e1, e2,  alternative = c("two.sided", "less", "greater"), h=1, power=2)
{
    alternative <- match.arg(alternative)
    d <- c(abs(e1))^power - c(abs(e2))^power
    d.cov <- acf(d, na.action=na.omit,lag.max = h-1, type = "covariance", plot=FALSE)$acf[,,1]
    d.var <- sum(c(d.cov[1], 2*d.cov[-1])) / length(d)
    STATISTIC <- mean(d,na.rm=TRUE)/sqrt(d.var)
    names(STATISTIC) <- "DM"
    if (alternative == "two.sided") 
      PVAL <- 2 * pnorm(-abs(STATISTIC))
    else if (alternative == "less") 
      PVAL <- pnorm(STATISTIC)
    else if (alternative == "greater") 
      PVAL <- pnorm(STATISTIC, lower.tail = FALSE)
    PARAMETER <- c(h,power)
    names(PARAMETER) <- c("Forecast horizon","Loss function power")
    structure(list(statistic = STATISTIC, parameter = PARAMETER, alternative = alternative, 
                   p.value = PVAL, method = "Diebold-Mariano Test", 
                   data.name =c(deparse(substitute(e1)),deparse(substitute(e2)))), 
              class = "htest")
}
