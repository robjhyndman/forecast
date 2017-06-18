#' @import parallel
#' @import Rcpp
#'
#' @importFrom colorspace sequential_hcl
#' @importFrom fracdiff fracdiff diffseries fracdiff.sim
#' @importFrom tseries adf.test pp.test kpss.test
#' @importFrom zoo rollmean as.Date as.yearqtr
#' @importFrom timeDate as.timeDate isBizday difftimeDate Easter as.Date.timeDate
#' @importFrom nnet nnet
#' @importFrom grDevices gray heat.colors nclass.FD
#' @importFrom graphics abline axis grid layout lines mtext par plot points polygon text title hist
#' @importFrom stats Box.test acf approx ar arima arima.sim as.ts complete.cases cycle decompose diffinv end extractAIC fitted formula frequency window filter na.contiguous spec.ar hatvalues is.ts ksmooth lm lsfit loess median model.frame na.exclude na.omit na.pass optim optimize pf plot.ts poly predict pt qnorm qt quantile residuals rnorm runif sd simulate smooth.spline start stl supsmu terms time ts tsp tsp<- tsdiag var logLik nobs napredict
#' @importFrom stats aggregate as.formula is.mts reformulate
#' @importFrom utils packageVersion tail head
#' @importFrom ggplot2 autoplot fortify
#' @importFrom lmtest bgtest
#' @importFrom magrittr %>%
#'
#' @useDynLib forecast, .registration = TRUE
NULL

#' @export
magrittr::`%>%`
