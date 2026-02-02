# Package index

## Package

Forecast package

- [`forecast-package`](https://pkg.robjhyndman.com/forecast/reference/forecast-package.md)
  : forecast: Forecasting Functions for Time Series and Linear Models

## Time series analysis

Functions for working with time series

- [`bizdays()`](https://pkg.robjhyndman.com/forecast/reference/bizdays.md)
  : Number of trading days in each season
- [`bld.mbb.bootstrap()`](https://pkg.robjhyndman.com/forecast/reference/bld.mbb.bootstrap.md)
  : Box-Cox and Loess-based decomposition bootstrap.
- [`BoxCox()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.md)
  [`InvBoxCox()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.md)
  : Box Cox Transformation
- [`BoxCox.lambda()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.lambda.md)
  : Automatic selection of Box Cox transformation parameter
- [`easter()`](https://pkg.robjhyndman.com/forecast/reference/easter.md)
  : Easter holidays in each season
- [`findfrequency()`](https://pkg.robjhyndman.com/forecast/reference/findfrequency.md)
  : Find dominant frequency of a time series
- [`fourier()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)
  [`fourierf()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)
  : Fourier terms for modelling seasonality
- [`is.constant()`](https://pkg.robjhyndman.com/forecast/reference/is.constant.md)
  : Is an object constant?
- [`monthdays()`](https://pkg.robjhyndman.com/forecast/reference/monthdays.md)
  : Number of days in each season
- [`msts()`](https://pkg.robjhyndman.com/forecast/reference/msts.md) :
  Multi-Seasonal Time Series
- [`na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.md)
  : Interpolate missing values in a time series
- [`ndiffs()`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.md)
  : Number of differences required for a stationary series
- [`nsdiffs()`](https://pkg.robjhyndman.com/forecast/reference/nsdiffs.md)
  : Number of differences required for a seasonally stationary series
- [`ocsb.test()`](https://pkg.robjhyndman.com/forecast/reference/ocsb.test.md)
  : Osborn, Chui, Smith, and Birchenhall Test for Seasonal Unit Roots
- [`seasonaldummy()`](https://pkg.robjhyndman.com/forecast/reference/seasonaldummy.md)
  [`seasonaldummyf()`](https://pkg.robjhyndman.com/forecast/reference/seasonaldummy.md)
  : Seasonal dummy variables
- [`subset(`*`<ts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/subset.ts.md)
  [`subset(`*`<msts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/subset.ts.md)
  : Subsetting a time series
- [`tsclean()`](https://pkg.robjhyndman.com/forecast/reference/tsclean.md)
  : Identify and replace outliers and missing values in a time series
- [`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md)
  : Identify and replace outliers in a time series

## Seasonal decomposition

Functions used in seasonal decomposition

- [`ma()`](https://pkg.robjhyndman.com/forecast/reference/ma.md) :
  Moving-average smoothing
- [`mstl()`](https://pkg.robjhyndman.com/forecast/reference/mstl.md) :
  Multiple seasonal decomposition
- [`seasonal()`](https://pkg.robjhyndman.com/forecast/reference/seasonal.md)
  [`trendcycle()`](https://pkg.robjhyndman.com/forecast/reference/seasonal.md)
  [`remainder()`](https://pkg.robjhyndman.com/forecast/reference/seasonal.md)
  : Extract components from a time series decomposition
- [`seasadj()`](https://pkg.robjhyndman.com/forecast/reference/seasadj.md)
  : Seasonal adjustment

## Modelling

Functions for estimating time series models

- [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  : Fit a fractionally differenced ARFIMA model
- [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md) :
  Fit ARIMA model to univariate time series
- [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  : Fit best ARIMA model to univariate time series
- [`baggedModel()`](https://pkg.robjhyndman.com/forecast/reference/baggedModel.md)
  [`baggedETS()`](https://pkg.robjhyndman.com/forecast/reference/baggedModel.md)
  : Forecasting using a bagged model
- [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) :
  BATS model (Exponential smoothing state space model with Box-Cox
  transformation, ARMA errors, Trend and Seasonal components)
- [`croston_model()`](https://pkg.robjhyndman.com/forecast/reference/croston_model.md)
  : Croston forecast model
- [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) :
  Exponential smoothing state space model
- [`mean_model()`](https://pkg.robjhyndman.com/forecast/reference/mean_model.md)
  : Mean Forecast Model
- [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  : Neural Network Time Series Forecasts
- [`rw_model()`](https://pkg.robjhyndman.com/forecast/reference/rw_model.md)
  : Random walk model
- [`spline_model()`](https://pkg.robjhyndman.com/forecast/reference/spline_model.md)
  : Cubic spline stochastic model
- [`stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.md) :
  Forecasting model using STL with a generative time series model
- [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md) :
  TBATS model (Exponential smoothing state space model with Box-Cox
  transformation, ARMA errors, Trend and Seasonal components)
- [`theta_model()`](https://pkg.robjhyndman.com/forecast/reference/theta_model.md)
  : Theta model
- [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md) :
  Fit a linear model with time series components

## Forecasting

Functions for producing forecasts

- [`forecast(`*`<croston_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md)
  [`croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md)
  : Forecasts for intermittent demand using Croston's method

- [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md) :
  Double-Seasonal Holt-Winters Forecasting

- [`ses()`](https://pkg.robjhyndman.com/forecast/reference/ses.md)
  [`holt()`](https://pkg.robjhyndman.com/forecast/reference/ses.md)
  [`hw()`](https://pkg.robjhyndman.com/forecast/reference/ses.md) :
  Exponential smoothing forecasts

- [`forecast(`*`<mean_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md)
  [`meanf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md)
  : Mean Forecast

- [`modelAR()`](https://pkg.robjhyndman.com/forecast/reference/modelAR.md)
  : Time Series Forecasts with a user-defined model

- [`forecast(`*`<rw_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  [`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  [`snaive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  : Naive and Random Walk Forecasts

- [`sindexf()`](https://pkg.robjhyndman.com/forecast/reference/sindexf.md)
  : Forecast seasonal index

- [`forecast(`*`<spline_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md)
  [`splinef()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md)
  :

  Returns local linear forecasts and prediction intervals using cubic
  smoothing splines estimated with
  [`spline_model()`](https://pkg.robjhyndman.com/forecast/reference/spline_model.md).

- [`forecast(`*`<stl>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  [`forecast(`*`<stlm>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  : Forecasting using stl objects

- [`forecast(`*`<theta_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.md)
  [`thetaf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.md)
  : Theta method forecasts.

- [`forecast(`*`<fracdiff>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  [`forecast(`*`<Arima>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  [`forecast(`*`<ar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  : Forecasting using ARIMA or ARFIMA models

- [`forecast(`*`<baggedModel>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.baggedModel.md)
  : Forecasting using a bagged model

- [`forecast(`*`<bats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md)
  [`forecast(`*`<tbats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md)
  : Forecasting using BATS and TBATS models

- [`forecast(`*`<ets>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md)
  : Forecasting using ETS models

- [`forecast(`*`<HoltWinters>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.HoltWinters.md)
  : Forecasting using Holt-Winters objects

- [`forecast(`*`<lm>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  : Forecast a linear model with possible time series components

- [`forecast(`*`<mlm>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.mlm.md)
  : Forecast a multiple linear model with possible time series
  components

- [`forecast(`*`<modelAR>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.modelAR.md)
  : Forecasting using user-defined model

- [`forecast(`*`<mts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.mts.md)
  : Forecasting time series

- [`forecast(`*`<nnetar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.nnetar.md)
  : Forecasting using neural network models

- [`forecast(`*`<StructTS>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.StructTS.md)
  : Forecasting using Structural Time Series models

- [`forecast(`*`<ts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
  [`forecast(`*`<default>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
  [`print(`*`<forecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
  : Forecasting time series

- [`is.forecast()`](https://pkg.robjhyndman.com/forecast/reference/is.forecast.md)
  [`is.mforecast()`](https://pkg.robjhyndman.com/forecast/reference/is.forecast.md)
  [`is.splineforecast()`](https://pkg.robjhyndman.com/forecast/reference/is.forecast.md)
  : Is an object a particular forecast type?

## Plotting

Functions for plotting time series and forecasts

- [`Acf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md)
  [`Pacf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md)
  [`Ccf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md)
  [`taperedacf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md)
  [`taperedpacf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md)
  : (Partial) Autocorrelation and Cross-Correlation Function Estimation
- [`checkresiduals()`](https://pkg.robjhyndman.com/forecast/reference/checkresiduals.md)
  : Check that residuals from a time series model look like white noise
- [`autoplot(`*`<acf>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  [`ggAcf()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  [`ggPacf()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  [`ggCcf()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  [`autoplot(`*`<mpacf>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  [`ggtaperedacf()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  [`ggtaperedpacf()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  : ggplot (Partial) Autocorrelation and Cross-Correlation Function
  Estimation and Plotting
- [`gghistogram()`](https://pkg.robjhyndman.com/forecast/reference/gghistogram.md)
  : Histogram with optional normal and kernel density functions
- [`gglagplot()`](https://pkg.robjhyndman.com/forecast/reference/gglagplot.md)
  [`gglagchull()`](https://pkg.robjhyndman.com/forecast/reference/gglagplot.md)
  : Time series lag ggplots
- [`ggseasonplot()`](https://pkg.robjhyndman.com/forecast/reference/seasonplot.md)
  [`seasonplot()`](https://pkg.robjhyndman.com/forecast/reference/seasonplot.md)
  : Seasonal plot
- [`ggmonthplot()`](https://pkg.robjhyndman.com/forecast/reference/ggmonthplot.md)
  [`ggsubseriesplot()`](https://pkg.robjhyndman.com/forecast/reference/ggmonthplot.md)
  : Create a seasonal subseries ggplot
- [`is.acf()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.Arima()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.baggedModel()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.bats()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.ets()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.modelAR()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.stlm()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.nnetar()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  [`is.nnetarmodels()`](https://pkg.robjhyndman.com/forecast/reference/is.ets.md)
  : Is an object a particular model type?
- [`autolayer(`*`<mts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  [`autolayer(`*`<msts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  [`autolayer(`*`<ts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  [`autoplot(`*`<ts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  [`autoplot(`*`<mts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  [`autoplot(`*`<msts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  [`fortify(`*`<ts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  : Automatically create a ggplot for time series objects
- [`plot(`*`<forecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  [`autoplot(`*`<forecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  [`autoplot(`*`<splineforecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  [`autolayer(`*`<forecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  [`plot(`*`<splineforecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  : Forecast plot
- [`autoplot(`*`<mforecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.mforecast.md)
  [`autolayer(`*`<mforecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.mforecast.md)
  [`plot(`*`<mforecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.mforecast.md)
  : Multivariate forecast plot
- [`autoplot(`*`<decomposed.ts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.seas.md)
  [`autoplot(`*`<stl>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.seas.md)
  [`autoplot(`*`<StructTS>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.seas.md)
  [`autoplot(`*`<seas>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.seas.md)
  [`autoplot(`*`<mstl>`*`)`](https://pkg.robjhyndman.com/forecast/reference/autoplot.seas.md)
  : Plot time series decomposition components using ggplot
- [`plot(`*`<Arima>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.Arima.md)
  [`plot(`*`<ar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.Arima.md)
  [`autoplot(`*`<Arima>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.Arima.md)
  [`autoplot(`*`<ar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.Arima.md)
  : Plot characteristic roots from ARIMA model
- [`plot(`*`<bats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.bats.md)
  [`autoplot(`*`<tbats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.bats.md)
  [`autoplot(`*`<bats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.bats.md)
  [`plot(`*`<tbats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.bats.md)
  : Plot components from BATS model
- [`plot(`*`<ets>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.ets.md)
  [`autoplot(`*`<ets>`*`)`](https://pkg.robjhyndman.com/forecast/reference/plot.ets.md)
  : Plot components from ETS model
- [`ggtsdisplay()`](https://pkg.robjhyndman.com/forecast/reference/tsdisplay.md)
  [`tsdisplay()`](https://pkg.robjhyndman.com/forecast/reference/tsdisplay.md)
  : Time series display
- [`StatForecast`](https://pkg.robjhyndman.com/forecast/reference/geom_forecast.md)
  [`GeomForecast`](https://pkg.robjhyndman.com/forecast/reference/geom_forecast.md)
  [`geom_forecast()`](https://pkg.robjhyndman.com/forecast/reference/geom_forecast.md)
  : Forecast plot

## Model analysis

Functions for analysing time series models

- [`arima.errors()`](https://pkg.robjhyndman.com/forecast/reference/arima.errors.md)
  : Errors from a regression model with ARIMA errors
- [`arimaorder()`](https://pkg.robjhyndman.com/forecast/reference/arimaorder.md)
  : Return the order of an ARIMA or ARFIMA model
- [`checkresiduals()`](https://pkg.robjhyndman.com/forecast/reference/checkresiduals.md)
  : Check that residuals from a time series model look like white noise
- [`getResponse()`](https://pkg.robjhyndman.com/forecast/reference/getResponse.md)
  : Get response variable from time series model.
- [`modeldf()`](https://pkg.robjhyndman.com/forecast/reference/modeldf.md)
  : Compute model degrees of freedom
- [`fitted(`*`<ARFIMA>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  [`fitted(`*`<Arima>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  [`fitted(`*`<ar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  [`fitted(`*`<bats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  [`fitted(`*`<ets>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  [`fitted(`*`<modelAR>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  [`fitted(`*`<nnetar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  [`fitted(`*`<tbats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/fitted.Arima.md)
  : h-step in-sample forecasts for time series models.
- [`residuals(`*`<forecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<ar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<Arima>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<bats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<tbats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<ets>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<ARFIMA>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<nnetar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<stlm>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  [`residuals(`*`<tslm>`*`)`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  : Residuals for various time series models
- [`tbats.components()`](https://pkg.robjhyndman.com/forecast/reference/tbats.components.md)
  : Extract components of a TBATS model

## Simulation

Functions for simulating time series data from fitted models

- [`simulate(`*`<ets>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<Arima>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<ar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<rw_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<fracdiff>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<nnetar>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<modelAR>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<tbats>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  [`simulate(`*`<spline_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  : Simulation from a time series model

## Forecast evaluation

Functions used for evaluating forecasts

- [`accuracy(`*`<forecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/accuracy.forecast.md)
  [`accuracy(`*`<mforecast>`*`)`](https://pkg.robjhyndman.com/forecast/reference/accuracy.forecast.md)
  [`accuracy(`*`<fc_model>`*`)`](https://pkg.robjhyndman.com/forecast/reference/accuracy.forecast.md)
  [`accuracy(`*`<Arima>`*`)`](https://pkg.robjhyndman.com/forecast/reference/accuracy.forecast.md)
  [`accuracy(`*`<lm>`*`)`](https://pkg.robjhyndman.com/forecast/reference/accuracy.forecast.md)
  [`accuracy(`*`<ts>`*`)`](https://pkg.robjhyndman.com/forecast/reference/accuracy.forecast.md)
  [`accuracy(`*`<numeric>`*`)`](https://pkg.robjhyndman.com/forecast/reference/accuracy.forecast.md)
  : Accuracy measures for a forecast model
- [`CV()`](https://pkg.robjhyndman.com/forecast/reference/CV.md) :
  Cross-validation statistic
- [`CVar()`](https://pkg.robjhyndman.com/forecast/reference/CVar.md) :
  k-fold Cross-Validation applied to an autoregressive model
- [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  : Diebold-Mariano test for predictive accuracy
- [`tsCV()`](https://pkg.robjhyndman.com/forecast/reference/tsCV.md) :
  Time series cross-validation

## Data

Data sets included in the package

- [`gas`](https://pkg.robjhyndman.com/forecast/reference/gas.md) :
  Australian monthly gas production
- [`gold`](https://pkg.robjhyndman.com/forecast/reference/gold.md) :
  Daily morning gold prices
- [`taylor`](https://pkg.robjhyndman.com/forecast/reference/taylor.md) :
  Half-hourly electricity demand
- [`wineind`](https://pkg.robjhyndman.com/forecast/reference/wineind.md)
  : Australian total wine sales
- [`woolyrnq`](https://pkg.robjhyndman.com/forecast/reference/woolyrnq.md)
  : Quarterly production of woollen yarn in Australia
