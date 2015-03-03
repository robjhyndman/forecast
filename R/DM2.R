#Diebold-Mariano test. Modified from code by Adrian Trapletti.
# Then adapted by M. Yousaf Khan for better performance on small samples

dm.test <- function (e1, e2, alternative = c("two.sided", "less", "greater"), h = 1, power = 2)
{
  alternative <- match.arg(alternative)
  d <- c(abs(e1))^power - c(abs(e2))^power
  d.cov <- acf(d, na.action = na.omit, lag.max = h - 1, type = "covariance", plot = FALSE)$acf[, , 1]
  d.var <- sum(c(d.cov[1], 2 * d.cov[-1]))/length(d)
  dv <- d.var#max(1e-8,d.var)
  if(dv > 0)
    STATISTIC <- mean(d, na.rm = TRUE) / sqrt(dv)
  else 
    stop("Variance of DM statistic is zero")
  
  n <- length(d)
  k <- ((n + 1 - 2 * h + (h/n) * (h-1))/n)^(1/2)
  STATISTIC <- STATISTIC * k
  names(STATISTIC) <- "DM"
  if (alternative == "two.sided")
    PVAL <- 2 * pt(-abs(STATISTIC), df = n-1)
  else if (alternative == "less")
    PVAL <- pt(STATISTIC, df = n-1)
  else if (alternative == "greater")
    PVAL <- pt(STATISTIC, df = n-1, lower.tail = FALSE)
  PARAMETER <- c(h, power)
  names(PARAMETER) <- c("Forecast horizon", "Loss function power")
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 alternative = alternative, p.value = PVAL, method = "Diebold-Mariano Test",
                 data.name = c(deparse(substitute(e1)), deparse(substitute(e2)))),
            class = "htest")
}
