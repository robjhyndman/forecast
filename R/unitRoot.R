#' Number of differences required for a stationary series
#'
#' Functions to estimate the number of differences required to make a given
#' time series stationary. \code{ndiffs} estimates the number of first
#' differences necessary.
#'
#' \code{ndiffs} uses a unit root test to determine the number of differences
#' required for time series \code{x} to be made stationary. If
#' \code{test="kpss"}, the KPSS test is used with the null hypothesis that
#' \code{x} has a stationary root against a unit-root alternative. Then the
#' test returns the least number of differences required to pass the test at
#' the level \code{alpha}. If \code{test="adf"}, the Augmented Dickey-Fuller
#' test is used and if \code{test="pp"} the Phillips-Perron test is used. In
#' both of these cases, the null hypothesis is that \code{x} has a unit root
#' against a stationary root alternative. Then the test returns the least
#' number of differences required to fail the test at the level \code{alpha}.
#'
#' @param x A univariate time series
#' @param alpha Level of the test, possible values range from 0.01 to 0.1.
#' @param test Type of unit root test to use
#' @param type Specification of the deterministic component in the regression
#' @param max.d Maximum number of non-seasonal differences allowed
#' @param ... Additional arguments to be passed on to the unit root test
#' @return An integer indicating the number of differences required for stationarity.
#' @author Rob J Hyndman, Slava Razbash & Mitchell O'Hara-Wild
#' @seealso \code{\link{auto.arima}} and \code{\link{ndiffs}}
#' @references 
#' Dickey DA and Fuller WA (1979), "Distribution of the Estimators for
#' Autoregressive Time Series with a Unit Root", \emph{Journal of the American
#' Statistical Association} \bold{74}:427-431.
#'
#' Kwiatkowski D, Phillips PCB, Schmidt P and Shin Y (1992) "Testing the Null
#' Hypothesis of Stationarity against the Alternative of a Unit Root",
#' \emph{Journal of Econometrics} \bold{54}:159-178.
#'
#' Osborn, D.R. (1990) "A survey of seasonality in UK macroeconomic variables",
#' \emph{International Journal of Forecasting}, \bold{6}:327-336.
#' 
#' Phillips, P.C.B. and Perron, P. (1988) "Testing for a unit root in time series regression",
#' \emph{Biometrika}, \bold{72}(2), 335-346.
#'
#' Said E and Dickey DA (1984), "Testing for Unit Roots in Autoregressive
#' Moving Average Models of Unknown Order", \emph{Biometrika}
#' \bold{71}:599-607.
#' @keywords ts
#' @examples
#' ndiffs(WWWusage)
#' ndiffs(diff(log(AirPassengers),12))
#'
#' @importFrom urca ur.kpss ur.df ur.pp
#' @export
ndiffs <- function(x,alpha=0.05,test=c("kpss","adf","pp"), type=c("level", "trend"), max.d=2, ...)
{
  test <- match.arg(test)
  type <- match(match.arg(type), c("level","trend"))
  x <- c(na.omit(c(x)))
  d <- 0
  
  if(alpha < 0.01){
    warning("Specified alpha value is less than the minimum, setting alpha=0.01")
    alpha <- 0.01
  }
  else if(alpha > 0.1){
    warning("Specified alpha value is larger than the maximum, setting alpha=0.1")
    alpha <- 0.1
  }
  
  if(is.constant(x))
    return(d)
  
  urca_pval <- function(urca_test){
    approx(urca_test@cval[1,], as.numeric(sub("pct", "", colnames(urca_test@cval)))/100, xout=urca_test@teststat[1], rule=2)$y
  }
  
  kpss_wrap <- function(..., use.lag = trunc(3*sqrt(length(x))/13)){
    ur.kpss(..., use.lag = use.lag)
  }
  
  runTests <- function(x, test, alpha){
    tryCatch(
      {suppressWarnings(
        diff <- switch(test,
                       kpss = urca_pval(kpss_wrap(x, type=c("mu","tau")[type], ...)) < alpha,
                       adf = urca_pval(ur.df(x, type=c("drift","trend")[type], ...)) > alpha,
                       pp = urca_pval(ur.pp(x, type="Z-tau", model=c("constant","trend")[type], ...)) > alpha,
                       stop("This shouldn't happen"))
      )
      diff
      },
      error = function(e){
        warning("The chosen test encountered an error, so no differencing is selected. Check the time series data.")
        FALSE
      }
    )
  }
  
  dodiff <- runTests(x, test, alpha)
  
  if(is.na(dodiff))
  {
    return(d)
  }
  while(dodiff && d < max.d)
  {
    d <- d+1
    x <- diff(x)
    if(is.constant(x))
      return(d)
    dodiff <- runTests(x, test, alpha)
    if(is.na(dodiff))
      return(d-1)
  }
  return(d)
}

# Number of seasonal differences
#' Number of differences required for a seasonally stationary series
#' 
#' Functions to estimate the number of differences required to make a given
#' time series stationary. \code{nsdiffs} estimates the number of seasonal differences 
#' necessary.
#'
#' \code{nsdiffs} uses seasonal unit root tests to determine the number of
#' seasonal differences required for time series \code{x} to be made stationary
#' (possibly with some lag-one differencing as well). 
#' 
#' Several different tests are available:
#' * If \code{test="seas"} (default), a measure of seasonal strength is used, where differencing is
#' selected if the seasonal strength (Wang, Smith & Hyndman, 2006) exceeds 0.64 
#' (based on minimizing MASE when forecasting using auto.arima on M3 and M4 data).
#' * If \code{test="ch"}, the Canova-Hansen (1995) test is used 
#' (with null hypothesis of deterministic seasonality) 
#' * If \code{test="hegy"}, the Hylleberg, Engle, Granger & Yoo (1990) test is used.
#' * If \code{test="ocsb"}, the Osborn-Chui-Smith-Birchenhall
#' (1988) test is used (with null hypothesis that a seasonal unit root exists).
#' 
#' @md
#' 
#' @inheritParams ndiffs
#' @param x A univariate time series
#' @param alpha Level of the test, possible values range from 0.01 to 0.1.
#' @param test Type of unit root test to use
#' @param m Deprecated. Length of seasonal period
#' @param max.D Maximum number of seasonal differences allowed
#' 
#' @return An integer indicating the number of differences required for stationarity.
#' 
#' @references 
#' 
#' Wang, X, Smith, KA, Hyndman, RJ (2006) "Characteristic-based clustering
#' for time series data", \emph{Data Mining and Knowledge Discovery},
#' \bold{13}(3), 335-364.
#' 
#' Osborn DR, Chui APL, Smith J, and Birchenhall CR (1988) "Seasonality and the
#' order of integration for consumption", \emph{Oxford Bulletin of Economics
#' and Statistics} \bold{50}(4):361-377.
#' 
#' Canova F and Hansen BE (1995) "Are Seasonal Patterns Constant
#' over Time? A Test for Seasonal Stability", \emph{Journal of Business and
#' Economic Statistics} \bold{13}(3):237-252.
#'
#' Hylleberg S, Engle R, Granger C and Yoo B (1990) "Seasonal integration
#' and cointegration.", \emph{Journal of Econometrics} \bold{44}(1), pp. 215-238.
#'
#' @author Rob J Hyndman, Slava Razbash and Mitchell O'Hara-Wild
#' 
#' @seealso \code{\link{auto.arima}}, \code{\link{ndiffs}}, \code{\link{ocsb.test}}, \code{\link[uroot]{hegy.test}}, and \code{\link[uroot]{ch.test}}
#'
#' @examples
#' nsdiffs(AirPassengers)
#'
#' @export
nsdiffs <- function(x, alpha = 0.05, m=frequency(x), test=c("seas", "ocsb", "hegy", "ch"), max.D=1, ...)
{
  test <- match.arg(test)
  D <- 0
  
  if(alpha < 0.01){
    warning("Specified alpha value is less than the minimum, setting alpha=0.01")
    alpha <- 0.01
  }
  else if(alpha > 0.1){
    warning("Specified alpha value is larger than the maximum, setting alpha=0.1")
    alpha <- 0.1
  }
  if(test == "ocsb" && alpha != 0.05){
    warning("Significance levels other than 5% are not currently supported by test='ocsb', defaulting to alpha = 0.05.")
    alpha <- 0.05
  }
  if(test %in% c("hegy", "ch")){
    if(!requireNamespace("uroot", quietly = TRUE)){
      stop(paste0("Using a ", test, ' test requires the uroot package. Please install it using `install.packages("uroot")`'))
    }
  }
  
  if(is.constant(x))
    return(D)
  
  if(!missing(m)){
    warning("argument m is deprecated; please set the frequency in the ts object.", 
            call. = FALSE)
    x <- ts(x, frequency = m)
  }
  
  if(frequency(x)==1)
    stop("Non seasonal data")
  else if(frequency(x) < 1)
  {
    warning("I can't handle data with frequency less than 1. Seasonality will be ignored.")
    return(0)
  }
  
  runTests <- function(x, test, alpha){
    tryCatch(
      {suppressWarnings(
        diff <- switch(test,
               seas = seas.heuristic(x, ...) > 0.64, # Threshold chosen based on seasonal M3 auto.arima accuracy.
               ocsb = with(ocsb.test(x, maxlag = 3, lag.method = "AIC", ...), statistics>critical),
               hegy = tail(uroot::hegy.test(x, deterministic = c(1,1,0), maxlag = 3, lag.method = "AIC", ...)$pvalues, 2)[-2] > alpha,
               ch = uroot::ch.test(x, type = "trig", ...)$pvalues["joint"] < alpha)
        )
        stopifnot(diff %in% c(0,1))
        diff
      },
      error = function(e){
        warning("The chosen test encountered an error, so no seasonal differencing is selected. Check the time series data.")
        0
      }
    )
  }
  
  dodiff <- runTests(x, test, alpha)
  
  if(dodiff && frequency(x) %% 1 != 0){
    warning("The time series frequency has been rounded to support seasonal differencing.", call. = FALSE)
    x <- ts(x, frequency = round(frequency(x)))
  }
  
  while(dodiff==1 && D < max.D)
  {
    D <- D + 1
    x <- diff(x, lag=frequency(x))
    if(is.constant(x))
      return(D)
    dodiff <- runTests(x, test, alpha)
  }
  return(D)
}

# Adjusted from robjhyndman/tsfeatures
seas.heuristic <- function(x){
  if ("msts" %in% class(x)) {
    msts <- attributes(x)$msts
    nperiods <- length(msts)
  }
  else if ("ts" %in% class(x)) {
    msts <- frequency(x)
    nperiods <- msts > 1
    season <- 0
  }
  else {
    stop("The object provided must be a time-series object (`msts` or `ts`)")
  }
  season <- NA
  stlfit <- mstl(x)
  remainder <- stlfit[, "Remainder"]
  seasonal <- stlfit[, grep("Season", colnames(stlfit)), drop = FALSE]
  vare <- var(remainder, na.rm = TRUE)
  nseas <- NCOL(seasonal)
  if (nseas > 0) {
    season <- numeric(nseas)
    for (i in seq(nseas)) season[i] <- max(0, min(1, 1 - vare/var(remainder + seasonal[, i], na.rm = TRUE)))

  }
  return(season)
}

# Model specification from Osborn DR, Chui APL, Smith J, and Birchenhall CR (1988) "Seasonality and the order of integration for consumption", Oxford Bulletin of Economics and Statistics 50(4):361-377.
# 
# $\Delta\Delta_m X_t = \beta_1Z_{4,t-1} + \beta_2Z_{5,t-m} + \alpha_1\Delta\Delta_mX_{t-1} + \ldots + \alpha_p\Delta\Delta_mX_{t-p}$
# Where $Z_{4,t} = \hat{\lambda}(B)\Delta_mX_t$, $Z_{5,t} = \hat{\lambda}(B)\Delta X_t$, and $\hat{\lambda}(B)$ is an AR(p) lag operator with coefficients from an estimated AR(p) process of $\Delta\Delta_m X_t$.

#' Osborn, Chui, Smith, and Birchenhall Test for Seasonal Unit Roots
#'
#' An implementation of the Osborn, Chui, Smith, and Birchenhall (OCSB) test.
#'
#' @inheritParams uroot::hegy.test
#' @aliases print.OCSBtest
#' @details 
#' The regression equation may include lags of the dependent variable. When lag.method = "fixed", the lag order is fixed to maxlag; otherwise, maxlag is the maximum number of lags considered in a lag selection procedure that minimises the lag.method criterion, which can be AIC or BIC or corrected AIC, AICc, obtained as AIC + (2k(k+1))/(n-k-1), where k is the number of parameters and n is the number of available observations in the model.
#'
#' Critical values for the test are based on simulations, which has been smoothed over to produce critical values for all seasonal periods.
#'
#' @return
#' ocsb.test returns a list of class "OCSBtest" with the following components:
#' * statistics the value of the test statistics.
#' * pvalues the p-values for each test statistics.
#' * method a character string describing the type of test.
#' * data.name a character string giving the name of the data.
#' * fitted.model	the fitted regression model.
#'
#' @references 
#' Osborn DR, Chui APL, Smith J, and Birchenhall CR (1988) "Seasonality and the
#' order of integration for consumption", \emph{Oxford Bulletin of Economics
#' and Statistics} \bold{50}(4):361-377.
#'   
#' @seealso \code{\link{nsdiffs}}
#'
#' @examples
#' ocsb.test(AirPassengers)
#' 
#' @importFrom stats AIC BIC
#' 
#' @export
ocsb.test <- function(x, lag.method = c("fixed", "AIC", "BIC", "AICc"), maxlag = 0)
{
  lag.method <- match.arg(lag.method)
  sname <- deparse(substitute(x))
  period <- round(frequency(x)) # Avoid non-integer seasonal period
  
  if(period == 1){
    stop("Data must be seasonal to use `ocsb.test`. Check your ts frequency.")
  }
  
  genLags <- function(y, maxlag){
    if(maxlag == 0){
      return(ts(numeric(NROW(y)), start = start(y), frequency = frequency(y)))
    }
    out <- do.call(cbind, lapply(seq_len(maxlag), function(k) stats::lag(y, -k)))
    if(NCOL(out) > 1){
      colnames(out) <- paste0("lag_", seq_len(maxlag))
    }
    return(out)
  }
  
  fitOCSB <- function(x, lag, maxlag){
    period <- round(frequency(x)) # Avoid non-integer seasonal period
    
    # Compute (1-B)(1-B^m)y_t
    y <- diff(diff(x, period))
    ylag <- genLags(y, lag)
    if(maxlag > 0){
      # Ensure models are fitted on same length for lag order selection via lag.method
      y <- tail(y, -maxlag)
    }
    mf <- na.omit(cbind(y=y, x=ylag))
    
    # Estimate lambda(B) coefficients
    ar.fit <- lm(y ~ 0 + ., data = mf)
    
    # Compute lambda(B)(1-B^m)y_{t-1}
    Z4_frame <- na.omit(cbind(y=diff(x, period), x=genLags(diff(x, period), lag)))
    Z4 <- Z4_frame[,"y"] - suppressWarnings(predict(ar.fit, Z4_frame))
    
    # Compute lambda(B)(1-B)y_{t-m}
    Z5_frame <- na.omit(cbind(y=diff(x), x=genLags(diff(x), lag)))
    Z5 <- Z5_frame[,"y"] - suppressWarnings(predict(ar.fit, Z5_frame))
    
    # Combine regressors
    data <- na.omit(cbind(mf, Z4 = stats::lag(Z4, -1), Z5 = stats::lag(Z5, -period)))
    y <- data[,1]
    xreg <- data[,-1]
    
    lm(y ~ 0 + xreg)
  }
  
  # Estimate maxlag
  if (maxlag > 0) {
    if (lag.method != "fixed"){
      tmp <- vector("list", maxlag + 1)
      fits <- lapply(seq_len(maxlag), function(lag) fitOCSB(x, lag, maxlag))
      icvals <- unlist(switch(lag.method,
                              AIC = lapply(fits, AIC),
                              BIC = lapply(fits, BIC),
                              AICc = lapply(fits, 
                                            function(x) {
                                              k <- x$rank + 1
                                              -2 * logLik(x) + 2 * k + (2 * k * (k + 1))/(length(residuals(x)) - k - 1)
                                            })
                              )
                       )
      id <- which.min(icvals)
      maxlag <- id - 1
    }
  }
  
  regression <- fitOCSB(x, maxlag, maxlag)
  #if(any(is.na(regression$coefficients)))
  #  stop("Model did not reach a solution. Check the time series data.")
  
  stat <- summary(regression)$coefficients[c("xregZ4", "xregZ5"), "t value"]
  
  if(any(is.na(stat))){
    stop("Model did not reach a solution. Consider using a longer series or a different test.")
  }
  
  structure(list(statistics = stat[2],
                 critical = calcOCSBCritVal(period),
                 method = "OCSB test",
                 lag.method = lag.method,
                 lag.order = maxlag,
                 fitted.model = regression,
                 data.name = sname),
            class = "OCSBtest")
}

# Return critical values for OCSB test at 5% level
# Approximation based on extensive simulations.
calcOCSBCritVal <- function(seasonal.period)
{
  log.m <- log(seasonal.period)
  return(-0.2937411*exp(-0.2850853*(log.m-0.7656451)+(-0.05983644)*((log.m-0.7656451)^2))-1.652202)
}

#' @export
print.OCSBtest <- function(x, ...){
  cat("\n")
  cat(strwrap(x$method, prefix = "\t"), sep = "\n")
  cat("\n")
  cat("data:  ", x$data.name, "\n\n", sep = "")
  cat(paste0("Test statistic: ", round(x$statistics, 4), ", 5% critical value: ", round(x$critical, 4)))
  cat("\n")
  cat("alternative hypothesis: stationary")
  cat("\n\n")
  cat(paste0("Lag order ", x$lag.order, " was selected using ", x$lag.method))
}