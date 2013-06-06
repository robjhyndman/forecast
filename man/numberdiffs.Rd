\name{ndiffs}
\alias{ndiffs}
\alias{nsdiffs}
\title{Number of differences required for a stationary series}
\usage{ndiffs(x, alpha=0.05, test=c("kpss","adf", "pp"))
nsdiffs(x, m=frequency(x), test=c("ocsb","ch"))
}
\arguments{
\item{x}{A univariate time series}
\item{alpha}{Level of the test}
\item{m}{Length of seasonal period}
\item{test}{Type of unit root test to use}
}

\description{Functions to estimate the number of differences required to make a given time series stationary. \code{ndiffs} estimates the number of first differences and \code{nsdiffs} estimates the number of seasonal differences.}

\details{\code{ndiffs} uses a unit root test to determine the number of differences required for time series \code{x} to be made stationary. If \code{test="kpss"}, the KPSS test is used with the null hypothesis that \code{x} has a stationary root against a unit-root alternative. Then the test returns the least number of differences required to pass the test at the level \code{alpha}. If \code{test="adf"}, the Augmented Dickey-Fuller test is used and if \code{test="pp"} the Phillips-Perron test is used. In both of these cases, the null hypothesis is that \code{x} has a unit root against a stationary root alternative. Then the test returns the least number of differences required to fail the test at the level \code{alpha}.

\code{nsdiffs} uses seasonal unit root tests to determine the number of seasonal differences required for time series \code{x} to be made stationary (possibly with some lag-one differencing as well). If \code{test="ch"}, the Canova-Hansen (1995) test is used (with null hypothesis of deterministic seasonality) and if \code{test="ocsb"}, the Osborn-Chui-Smith-Birchenhall (1988) test is used (with null hypothesis that a seasonal unit root exists).}

\seealso{\code{\link{auto.arima}}}

\references{
Canova F and Hansen BE (1995) "Are Seasonal Patterns Constant over Time? A Test for Seasonal Stability", \emph{Journal of Business and Economic Statistics} \bold{13}(3):237-252.

Dickey DA and Fuller WA (1979), "Distribution of the Estimators for Autoregressive Time Series with a Unit Root", \emph{Journal of the American Statistical Association} \bold{74}:427-431.

Kwiatkowski D, Phillips PCB, Schmidt P and Shin Y (1992) "Testing the Null Hypothesis of Stationarity against the Alternative of a Unit Root", \emph{Journal of Econometrics} \bold{54}:159-178.

Osborn DR, Chui APL, Smith J, and Birchenhall CR (1988) "Seasonality and the order of integration for consumption", \emph{Oxford Bulletin of Economics and Statistics} \bold{50}(4):361-377.

Osborn, D.R. (1990) "A survey of seasonality in UK macroeconomic variables", \emph{International Journal of Forecasting}, \bold{6}:327-336.

Said E and Dickey DA (1984), "Testing for Unit Roots in Autoregressive Moving Average Models of Unknown Order", \emph{Biometrika} \bold{71}:599-607.
}

\value{An integer.}

\author{Rob J Hyndman and Slava Razbash}

\examples{ndiffs(WWWusage)
nsdiffs(log(AirPassengers))
ndiffs(diff(log(AirPassengers),12))
}

\keyword{ts}
