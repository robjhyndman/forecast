# forecast 8.21.1
  * nnetar now allows p or P to be 0
  * Bug fixes and improved docs

# forecast 8.21
  * Fixed df calculation for Ljung-Box tests in checkresiduals
  * Fixed some broken tests

# forecast 8.20
  * Improvements to unit tests, and migrate to testthat 3e
  * Prevent failure in C23 mode

# forecast 8.19
  * Bug fixes

# forecast 8.18
  * Updated RW forecasts to use an unbiased estimate of sigma2
  * Bug fixes

# forecast 8.17.0
  * Updated dm.test() to add alternative variance estimators. (#898)
  * Added `simulate.tbats()` for simulating from TBATS models.
  * Added dependency on generics for accuracy() and forecast() (#902)
  * Bux fixes

# forecast 8.16
  * Fixed `tslm()` incorrectly applying Box-Cox transformations when an `mts`
is provided to the `data` argument (#886).
  * Set D=0 when auto.arima applied to series with 2m observations or fewer.
  * Improved performance of parallel search of ARIMA models (jonlachmann, #891).
  * Fixed scoping of functions used in `ggAcf()` (#896).
  * Fixed checks on xreg in `simulate.Arima()` (#818)
  * Improved docs and bug fixes.

# forecast 8.15
  * Changed `summary()` methods to defer console output until `print()`
  * Changed default `s.window` values for `mstl()`, `stlf()` and `stlm()`. The new defaults are based on extensive empirical testing.

# forecast 8.14
  * Changed default `BoxCox(lambda = "auto")` lower bound to -0.9.
  * Use better variance estimates for `ets()` bias adjustments.
  * Improved robustness of `autoplot.seas()` for non-seasonal decomposition.
  * Fixed scoping of parameters in `auto.arima(parallel = TRUE)` (#874).
  * Fixed handling of `xreg` in `tsCV()`.

# forecast 8.13
  * Fixed forecasts from Arima with drift with initial NAs.
  * Fixed season colours in `gglagplot()` to match y-axis (original data).
  * Fixed facet order for classical decomposition `autoplot()`
  * Fixed `summary()` erroring for `tslm()` models containing NA values.

# forecast 8.12
  * Fixed bias adjusted forecast mean for ARIMA forecasts.
  * Improved naming of `accuracy()` generic formals.
  * Fix seasonal periods for `taylor` dataset.

# forecast 8.11
  * The axis for `gglagplot()` have been reversed for consistency with `stats::lag.plot()`.

# forecast 8.10
  * Updates to remove new CRAN errors
  * Bug fixes

# forecast 8.9
  * Updates for CRAN policies on Suggests packages
  * Bug fixes

# forecast 8.8
  * Updates for compatibility with fable
  * Bug fixes

# forecast 8.7
  * Documentation improvements
  * Bug fixes

# forecast 8.6
  * Reduced conflicts with tidy forecasting packages
  * Forecast autoplots now use same colour shading as autolayer() and geom_forecast
  * Documentation improvements
  * Bug fixes

# forecast 8.5
  * Updated tsCV() to handle exogenous regressors
  * Reimplemented lagwalk methods (naive, snaive, rwf) for speed improvements
  * Added support for passing arguments to auto.arima() unit root tests
  * Improved auto.arima() stepwise search algorithm
  * Documentation improvements
  * Bug fixes

# forecast 8.4
  * Added modelAR(), generalising nnetar() to support user-defined functions
  * Added na.action argument to ets
  * Documentation improvements
  * Bug fixes

# forecast 8.3
  * Added mstl() to handle multiple seasonal decomposition
  * stlf(), stlm(), tsoutliers() and tsclean() all now use mstl().
  * Updated tsCV() to handle multiple horizons
  * Switched unit root tests in ndiffs() to use urca package
  * Added ocsb.test
  * Changed method for choosing D in auto.arima() to a measure of seasonal strength.
  * Added baggedModel() function to generalize baggedETS
  * Added bootstrapped PI to more functions
  * Allowed lambda='auto' for all functions with lambda argument.
  * Updated author list to include all major contributors
  * Documentation improvements
  * Bug fixes

# forecast 8.2
  * Added pkgdown site
  * Added rolling window option to tsCV
  * Improved robustness to short time series and missing values
  * Bug fixes

# forecast 8.1
  * Added as.character.ets, as.character.bats, as.character.tbats
  * Made gghistogram() and checkresiduals() robust to missing values
  * All documentation now generated using roxygen
  * Improved documentation for many functions
  * Added autoplot.msts() and autolayer.msts
  * Added as.character methods for many models to generate model names
  * Added as.ts.forecast
  * autoplot method for bats/tbats models
  * Better ARIMA trace output
  * Made accuracy an S3 method
  * Bug fixes

# forecast 8.0
  * Added tips to start up message
  * Added pipe operator
  * Added tsCV() and CVar() functions
  * Added baggedETS
  * Added head.ts() and tail.ts(), so head and tail now work properly on ts objects.
  * Added gghistogram() and checkresiduals
  * Added ggseasonplot with polar coordinates
  * Modified defaults for gglagplot
  * Added autolayer.ts
  * Added type argument to residuals() for different types of residuals
  * Added support for seas objects from the seasonal package
  * Component extraction for seasonal decomposition methods
  * Range bars for decomposition autoplots
  * Added autoplot.StructTS
  * Added vignette based on 2008 JSS article by Hyndman and Khandakar
  * Improved ggplot functions
  * mforecast objects re-structured
  * Added as.data.frame.mforecast
  * autoplot functions now exported
  * Refit support for arfima() and stlm
  * Better bias adjustment support after Box-Cox transformation
  * print.ARIMA has better labelling of constants
  * Bug fixes
  * Removed fortify method for forecast objects

# forecast 7.3
  * Added prediction intervals and simulation for nnetar().
  * Documentation improvement
  * Bug fixes

# forecast 7.2
  * Faceting for autoplot.mts
  * Box-Cox support for ses, holt, hw
  * ets() now works for tiny time series
  * Added h-step fitted values in fitted() function.
  * seasonal adjustment added to thetaf
  * y now the standard first argument in all modelling functions
  * Added truncate argument to auto.arima
  * seasadj() now an S3 method
  * series with frequency < 1 and non-integer seasonality now handled better
  * ggplot2 theme support
  * Added gglagplot, gglagchull
  * Arima() and auto.arima() now allow any argument to be passed to stats::arima().
  * Bug fixes and speed improvements

# forecast 7.1
  * Fixed bug in auto.arima where the Box-Cox transformation was sometimes applied twice
  * Improved axes for ggseasonalplot
  * Improved tslm() to avoid some problems finding data
  * nnetar() updated to allow subsets
  * Modified initial values for ets
  * Improved unit tests to avoid deprecated functions and to avoid data from fpp
  * Removed fpp from Suggests list

# forecast 7.0
  * Added ggplot2 graphics
  * Bias adjustment option added for all functions that allow Box-Cox transformations
  * Added Ccf function, and rewrote Acf to handle multivariate series.
  * tslm() completely rewritten to be more robust and to handle fourier terms more easily
  * Support for multivariate linear models added
  * subset.ts() more robust, and captures some errors.
  * Added xreg argument to nnetar
  * Improved labels in seasonplot
  * More unit tests added
  * Documentation improvements
  * Bug fixes

# forecast 6.2
  * Many unit tests added using testthat.
  * Fixed bug in ets when very short seasonal series were passed in a data frame.
  * Fixed bug in nnetar where the initial predictor vector was reversed.
  * Corrected model name returned in nnetar().
  * Fixed bug in accuracy() when non-integer seasonality used.
  * Made auto.arima() robust to non-integer seasonality.
  * Fixed bug in auto.arima where allowmean was ignored when stepwise=FALSE.
  * Improved robustness of forecast.ets() for explosive models with multiplicative trends.
  * Exogenous variables now passed to VAR forecasts
  * Increased maximum nmse in ets() to 30.
  * Made tsoutliers() more robust to weak seasonality
  * Changed tsoutliers() to use supsmu on non-seasonal and seasonally adjusted data.
  * Fixed bug in tbats() when seasonal period 1 is a small multiple of seasonal period 2.
  * Other bug fixes

# forecast 6.1
  * Made auto.arima more robust

# forecast 6.0
  * Modified dm.test to give error when variance is zero
  * Corrected help file for splinef().
  * Fixed typo in accuracy help file regarding RMSE
  * Fixed bug in accuracy() which occurred with Arima and ets objects.
  * Fixed arima.errors() to handle Box-Cox transformed models.
  * Modified auto.arima() to be stricter on near-unit-roots.
  * Added allowmean argument in auto.arima().
  * Improved handling of constant series in Arima() and forecast.Arima().
  * Added plot.Arima() and plot.ar() functions.
  * Added as.character.Arima
  * Captured problem in bats/tbats where data are constant.
  * Modified TBATS and BATS estimation to avoid occasional instabilities.
  * Fixed bug in forecasts from bats which labelled them as TBATS.
  * Added allow.multiplicative.trend argument to ets().
  * Set allow.multiplictive.trend=FALSE in stlf(), stlm() and forecast.ts().
  * Simplified arguments in stlf().
  * Added taperedacf and taperedpacf functions
  * Added functions for bootstrapping time series

# forecast 5.9
  * Improved documentation of accuracy() function.
  * Fixed occasional bug in accuracy() when test set is a single observation.
  * Improved Acf() to give better handling of horizontal axis for seasonal data or when ... is passed.
  * Removed print.Arima and predict.Arima and added print.ARIMA
  * method argument now passed when re-fitting an ARIMA model.
  * Fixed error when CH test applied to short series

# forecast 5.8
  * Fixed bug in versions of R before 3.10 when using fourier and fourierf.
  * Made BoxCox.lambda() robust to missing values.

# forecast 5.7
  * Fixed bug in tbats/bats where optional arguments were not being passed to auto.arima().
  * Revised fourier() and fourierf() to avoid large orders, and to avoid zero columns.
  * Improved accuracy of fourier() and fourierf(), while simplifying the code.
  * Removed duplicate columns returned by fourier/fourierf with multiple seasonal periods.
  * Corrected some bugs in simulate.Arima for models involving xreg.
  * Centred simulations from simulate.Arima for non-stationary models by conditioning on first observation.
  * Added findfrequency() function.
  * Fixed error in computed residuals from forecast.stl().
  * Improved handling of very short series in auto.arima().
  * Fixed error in forecasting with additive damped models. Damping previously applied only from second forecast horizon.
  * Fixed misuse of abs() in two places in C code.
  * Added na.action argument to Acf() and fixed na.action argument in tsdisplay().

# forecast 5.6
  * Improved tbats and bats by ensuring ARMA coefficients are not close to the
    boundary of invertibility and stationarity.
  * Improved nsdiffs() handling of degenerate series (e.g., all zeros).
  * Improved forecast.ar() when function buried within other functions.
  * Improved handling of degenerate ARIMA models when xreg used.
  * More robust ets() initialization.
  * Fixed problem in na.interp() with seasonal data having frequency <= 5.
  * Removed undocumented option to use Rmalschains for optimization of ets().

# forecast 5.5
  * Improved documentation for croston
  * Added stlm() and forecast.stlm() functions, and added forecastfunction argument as a way of
    specifying a forecast method in stlf() and forecast.stl().
  * Improved forecast.ar() so that it is more likely to work if ar() and forecast.ar() are
    embedded within other functions.
  * Improved handling of ARIMA models with seasonality greater than 48
  * Improved handling of some degenerate regression models in nsdiffs
  * Changed AIC for poor models from 1e20 to Inf.
	* Update fourier() and fourierf() to work with msts object.
	* Added a new argument find.frequency to forecast.ts().
	* Added new arguments d and D to accuracy() for MASE.
  * Corrected bugs in accuracy().
  * Better handling of regression models with perfect fit in auto.arima().
  * Fixed bug in tbats.components() when there are no seasonal components.

# forecast 5.4
  * Fixed bug in forecast.tbats() and forecast.bats() when ts.frequency does not match seasonal.periods.
  * Fixed bug in getResponse.lm() when there's a logged dependent variable.
  * Modified ets() to avoid problems when data contains large numbers.
  * Modified ets() to produce forecasts when the data are constant.
  * Improved arima.errors() to find xreg more often, and to return an error if it can't be found.

# forecast 5.3
  * Unit tests added
  * Fixed bug in zzhw() which reversed the sign of the residuals.
  * Updated help file for CV() to specify it is only leave-one-out.
  * Fixed guer.cv() to allow non-integer periods without warning.
  * Added use.initial.values argument in ets().
  * Added arimaorder() function.
  * Modified warnings suppression by using suppressWarnings() throughout.

# forecast 5.2
  * Changed default number of cores to 2 for all functions that use parallel processing.
  * Removed remaining call to bats() from examples that are run.

# forecast 5.1
  * Fixed bug in tsoutliers() and tsclean() with very short seasonal series.
  * Fixed bug in Arima() when seasonal order is specified numerically instead of via a list.
  * Removed dimension attribution from output of arima.errors
  * Improved handling of "test" in accuracy
  * Changed parallel processing to parLapply for auto.arima
  * Added timeDate dependency to avoid errors in easter() and link to Rcpp >= 0.11.0.

# forecast 5.0
  * Added argument model to dshw().
  * Added bizdays() and easter() for calendar variables.
  * Added arguments max.D and max.d to auto.arima(), ndiffs() and nsdiffs().
  * Made several functions more robust to zoo objects.
  * Corrected an error in the calculation of AICc when using CV().
  * Made minimum default p in nnetar equal to 1.
  * Added tsoutliers() and tsclean() for identifying and replacing outliers
  * Improved na.interp() to handle seasonality and added argument lambda to na.interp
  * Added robust option to forecast.ts() to allow outliers and missing values
  * Improved output from snaive() and naive() to better reflect user expectations
  * Allowed Acf() to handle missing values by using na.contiguous
  * Changed default information criterion in ets() to AICc.
  * Removed drift term in Arima() when d+D>1.
  * Added bootstrap option to forecast.Arima

# forecast 4.8
  * Fixed bug in rwf() that was introduced in v4.7

# forecast 4.7
  * Added forecast.forecast() to simply return the object that is passed.
  * Removed leading zero in package number. i.e., 4.7 instead of 4.07.
  * better handling of nearly constant time series, and nearly linear time series
  * improved handling of missing values in rwf
  * corrected fitted values and residuals in meanf() for time series data
  * bats() and tbats() now handle missing values in the same way as ets(). i.e., using longest contiguous portion.
  * better handling of very short time series
  * initial states for ets() modified for very short time series (less than 3 years).
  * nsdiffs with CH test now handles degenerate cases without returning an error.
  * nnetar now handles missing values
  * Fixed bug in forecast.varest() so residuals and fitted values computed correctly.
  * Added accuracy() calculation for VAR models
  * Fixed a bug in simulate.fracdiff() when future=TRUE. Sometimes the future argument was being ignored.

# forecast 4.06
  * accuracy() was returning a mape and mpe 100 times too large for in-sample errors.

# forecast 4.05
  * Fixed bug in hw() so it works when initial="simple"
  * Allowed bats() and tbats() to take non-positive values.
  * ets() now calls optim direct via c code making ets() run much faster.
  * Added Rmalschains as a possible optimizer in ets(). Not documented.
  * Modified forecast.lm so it is more likely that the original data are stored in the returned object.
  * Corrected bug in forecast.Arima that occurred when a Box-Cox transformation was used with bootstrap=TRUE.
  * accuracy() updated so that it gives more information, and returns a matrix of both test and training measures.
  * Corrected training error measures for splinef() forecasts.

# forecast 4.04
  * Added ylim argument to Acf
  * Avoided clash with the signal package when using auto.arima().
  * Fixed problem in plot.forecast() when all historical data are NA or when there is no available historical data.
  * forecast.Arima() is now a little more robust if a zoo object is passed instead of a ts object.
  * CV() now handles missing values in the residuals.
  * Fixed bug in holt() and hw() so that the printed model no longer contains missing values.

# forecast 4.03
  * forecast.lm now guesses the variable name if there is only one predictor variable.
  * Removed error trap in forecast.lm when no xreg variables passed as it was catching legitimate calls.

# forecast 4.02
  * Fixed error in the prediction intervals returned by forecast.ets() when simulation was used and a Box-Cox transformation was specified.
  * Fixed bug in accuracy() when a numerical f vector was passed.
  * Fixed man file for Diebold-Mariano test.
  * Corrected references in nsdiffs() help page.
  * Added warning to nsdiffs when series too short for seasonal differencing.
  * Fixed problem in getResponse.Arima when Arima object created by stats::arima() from within a function.
  * Added tbats.components() and extended seasadj() to allow tbats objects.
  * Added undocumented functions for forecasting, printing and plotting output from vars::VAR.

# forecast 4.01
  * Error now trapped when newxreg variables not passed to forecast.lm
  * Corrected help file for dshw() to remove references to prediction intervals.
  * Improved help file for dm.test() to give more information about the alternative hypotheses.
  * Improved dm.test() performance for small samples by using a t-distribution instead of normal.
  * Modified bats() and tbats() examples to follow CRAN policies on parallel processing.
  * Moved some packages from Depends to Imports.
  * Added getResponse() function to return the historical time series from various time series model objects.
  * Modified accuracy() to use getResponse().
  * Allowed user-generated innovations in simulate.ets(), simulate.Arima(), etc.
  * Allowed xreg argument in forecast.stl() and stlf() when ARIMA model used.
  * Removed reliance on caret, and associated fitted and residuals functions.

# forecast 4.00
  * More robust handling of degenerate ARIMA models.
  * New defaults for shaded colors used for prediction intervals in plots.
  * auto.arima() now remembers the name of the series when a Box-Cox transformation is used.
  * New function nnetar() for automatic neural network forecasting of time series.
  * arfima() now tries harder to ensure the ARMA part is stationary.
  * ts control added for forecast of linear models in forecast.lm().
  * Fixed bug in bats() which caused an error when use.box.cox=FALSE and use.trend=FALSE.
  * Added residuals and fitted methods for train and avNNet objects from caret package.
  * accuracy() can now figure out overlapping times for x and f.
  * rwf() now handles missing values.
  * Revised ses(), holt() and hw() so that they can optionally use traditional initialization.

# forecast 3.25
  * Fixed bug in simulate.Arima.
  * Improved handling of short seasonal time series in auto.arima().
  * Added seasonal argument to auto.arima().
  * Fixed bug in splinef() and added gcv method for estimating smoothing parameter.

# forecast 3.24 (23 July 2012
  * Fixed bug in auto.arima() introduced in v3.23 which meant a ARIMA(0,0,0) model was
    returned about half the time.

# forecast 3.23
  * Fixed bug in arfima() which meant the drange argument was being ignored.
  * Extended auto.arima() so it returns something sensible when the data are constant.

# forecast 3.22
  * Increased maximum forecast horizon for ets models from 2000 to unlimited.
  * Corrected bug in Arima(). Previously include.constant=FALSE was ignored.
  * Some corrections to bats and tbats.
  * Modified parallel implementation in auto.arima for Windows.

# forecast 3.21
  * Fixed bug in auto.arima() when lambda is non-zero and stepwise is FALSE.
  * Fixed bug in auto.arima() in selecting d when D>0.
  * Fixed bug in ets() when seasonal period is less than 1.
  * Turned off warnings in auto.arima() and ets() when seasonal period is less than 1.
  * Added plotting methods for bats and tbats objects.
  * Changed default forecast horizons for bats and tbats objects.
  * Modified bats and tbats so they now use seasonal.periods when ts and msts objects are being modelled.

# forecast 3.20
  * Fixed bugs in forecast.lm().
  * Improved handling of newdata in forecast.lm() to provide more meaningful error messages.
  * Fixed bug in dm.test() that occurred when errors were very small.

# forecast 3.19
  * Improved plotting of forecast objects from lm models
  * Added MASE for lm forecasts using insample mean forecasts for scaling.
  * Modified definition of MASE for seasonal time series to use seasonal naive insample scaling.
  * Modified meanf() to allow it to be used with cross-sectional data.
  * Updated accuracy() to allow it to be used with cross-sectional data, lm forecasts and lm objects.

# forecast 3.18
  * Added method for plotting non-time-series forecasts to plot.forecast().
  * Removed partial arg matching.
  * Cleaned up some code, removing commented out sections, etc.
  * Added robust option to stlf().
  * Added naive and rwdrift options to stlf() and forecast.stl().
  * Improved handling of msts objects in BoxCox.lambda
  * Fixed some minor bugs in tbats() and bats
  * Improved speed of bats() and tbats().

# forecast 3.17
  * Improved forecast.lm() so it is more likely to find the original data from an lm object.
  * Parallel processing now available in auto.arima() when stepwise=FALSE
  * Default model selection in auto.arima() changed to AICc rather than AIC. This may affect model selection for very short time series.
  * max orders in auto.arima() now restricted to be less than 1/3 of length of data.

# forecast 3.16
  * Corrected problem with AIC computation in bats and tbats
  * Fixed handling of non-seasonal data in bats
  * Changed dependency to >= R 2.14.0 in order to ensure parallel package available.

# forecast 3.15
  * New functions tbats() and forecast.tbats() for multiple seasonal time series modelling.
  * bats() and tbats() use parallel processing when possible.
  * Minor improvements to bats() and forecast.bats().
  * decompose() removed as the function in the stats package has now been fixed.

# forecast 3.14
  * Improved documentation for forecast.ts
  * Corrected bug in dshw() when applied to a non-ts object.
  * Added error message when dshw() applied to data containing zeros or negative values
  * Added checks when dshw() applied to time series with non-nested periods.
  * Added msts object class for multiple seasonal time series
  * Made taylor data set an msts object.
  * Added bats() function for multiple seasonal time series modelling
  * Added forecast.bats() function for forecasting BATS models
  * Byte compiling turned on
  * Depending on Rcpp and RcppArmadillo to speed some code up.

# forecast 3.13
  * Bug fix for forecast.StructTS() due to changes in the StructTS object. The default h was being set to 0.
    Thanks to Tarmo Leinonen for reporting this problem.
  * Bug fix for forecast.stl() where h longer than one seasonal period sometimes returned missing forecasts.
    Thanks to Kevin Burton for reporting this problem.
  * forecast.stl() no longer allows a seasonal ETS model to be specified. Thanks to Stefano Birmani for the suggestion.

# forecast 3.12
  * Added option to control ets model in stlf() and forecast.stl(). Thanks to Stefano Birmani for the suggestion.
  * Reordered arguments for forecast.lm() and stlf() to be consistent with other forecast functions.
  * Modified tslm() so that it is more likely to find the relevant data when it is not passed as an argument.
  * Fixed bug in forecast.ets which returned all zero forecasts for some models when seasonal period > 24.

# forecast 3.11
  * Fixed bug in dshw() when smallest period is odd

# forecast 3.10
  * Added lambda argument to naive() and snaive().
  * Fixed bug in ets() with high frequency data.
  * Fixed bug in rwf() where incorrect fitted values and residuals were sometimes returned.
  * Modified number of lags displayed by default in tsdisplay().

# forecast 3.09
  * Fixed bug causing occasional problems in simulate.Arima() when MA order greater than 2 and future=TRUE.

# forecast 3.08
  * Bug fix in forecast.stl() which occurred when forecast horizon is less than seasonal period.
  * Added lambda argument to forecast.stl().

# forecast 3.07
  * Bug fix in ets() concerning non-seasonal models and high-frequency data. It sometimes returned all forecasts equal to zero.

# forecast 3.06
  * Switched to useDynLib in preparation for Rv2.14.0.

# forecast 3.05
  * Fixed bug in ets() which prevent non-seasonal models being fitted to high frequency data.

# forecast 3.04
  * Fixed bug when drift and xreg used together in auto.arima() or Arima().

# forecast 3.03
  * Bug fix in dshw() which was using slightly incorrect seasonal estimates for the forecasts
  * Bug fix in forecast.StructTS due to change in structure of StructTS object.
  * Better error capture in tslm when seasonal dummies are specified for non-seasonal data.
  * Re-formatted some help files to prevent viewing problems with the pdf manual.

# forecast 3.02
  * Bug fixes

# forecast 3.00
  * Added Box-Cox parameter as argument to Arima(), ets(), arfima(), stlf(), rwf(), meanf(), splinef
  * Added Box-Cox parameter as argument to forecast.Arima(), forecast.ets(), forecast.fracdiff(), forecast.ar(), forecast.StructTS, forecast.HoltWinters().
  * Removed lambda argument from plot.forecast() and accuracy().
  * Added BoxCox.lambda() function to allow automatic choice for Box-Cox parameter using Guerrero's method or the profile log likelihood method.
  * Modified BoxCox and InvBoxCox to return missing values when lambda < 0 and data < 0.
  * Add nsdiffs() function for selecting the number of seasonal differences.
  * Modified selection of seasonal differencing in auto.arima().
  * Better error message if seasonal factor used in tslm() with non-seasonal data.
  * Added PI argument to forecast.ets() to allow only point forecasts to be computed.
  * Added include.constant argument to Arima().
  * Added subset.ts() function.
  * Upgraded seasonplot() function to allow colors and to fix some bugs.
  * Fixed fitted values returned by forecast.HoltWinters
  * Modified simulate.Arima() because of undocumented changes in filter() function in stats package.
  * Changed residuals returned by splinef() to be ordinary residuals. The standardized residuals are now returned as standardizedresiduals.
  * Added dshw() function for double-seasonal Holt-Winters method based on Taylor (2003).
  * Fixed further bugs in the decompose() function that caused the results to be incorrect with odd frequencies.

# forecast 2.19
  * Added xreg information to the object returned by auto.arima().
  * Added Acf(), Pacf(), ma() and CV() functions.
  * Fixed bugs in re-fitting ARIMA models to new data.

# forecast 2.18 (2011-05-19)
  * Fixed bug in seasonplot() where year labels were sometimes incorrect.

# forecast 2.17
  * Modified simulate.Arima() to handle seasonal ARIMA models.
  * Modified ets() to handle missing values. The largest continuous section of data is now modelled.
  * Improved plot.forecast() to handle missing values at the end of the observed series.
  * Added replacement decompose() to avoid truncation of seasonal term and seasonally adjusted series.
  * Fixed bug in seasadj() to handle multiplicative decomposition, and to avoid missing values at ends.

# forecast 2.16
  * Changed the way missing values are handled in tslm

# forecast 2.15
  * Added fourier(), fourierf(), tslm
  * Improved forecast.lm() to allow trend and seasonal terms.

# forecast 2.14
  * Added forecast.lm
  * Modified accuracy() and print.forecast() to allow non time series forecasts.
  * Fixed visibility of stlf().

# forecast 2.13
  * Fixed bug in accuracy() when only 1 forecast is specified.
  * Added forecast.stl() and stlf() functions
  * Modified forecast.ts() to use stlf() if frequency > 12.
  * Made BoxCox() and InvBoxCox() robust to negative values
  * Fixed bug in simulate.Arima() when future=TRUE. There was a bias in the sample paths.

# forecast 2.12
  * Added naive() and snaive() functions.
  * Improved handling of seasonal data with frequency < 1.
  * Added lambda argument to accuracy().

# forecast 2.11
  * If MLE in arfima() fails (usually because the series is non-stationary), the LS estimate is now returned.

# forecast 2.10
  * Fixed bug in arfima() where the MA parameters were of the wrong sign if estim="mle" chosen.
  * arfima() now allowed to have a sequence of missing values at the start of the series and end of the series

# forecast 2.09
  * Fixed bug in forecast.fracdiff() which caused an error when h=1.
  * Added shadebars to plot.forecast().
  * Fixed bug in plot.forecast() to allow plotting when h=1.

# forecast 2.08
  * Added pp test option for auto.arima() and ndiffs().
  * Fixed bug in simulate.ets() which was causing problems when forecasting from some ETS models including ETS(M,M,N).

# forecast 2.07
  * Fixed bug in simulate.Arima(). Previous sample paths when d=2 and future=TRUE were incorrect.
  * Changed way color is implemented in plot.forecast() to avoid colour changes when the graphics window is refreshed.

# forecast 2.06
  * Added MLE option for arfima().
  * Added simulate.Arima(), simulate.ar() and simulate.fracdiff

# forecast 2.05
  * Added arfima() and a forecast method to handle ARFIMA models from arfima() and fracdiff().
  * Added residuals and fitted methods for fracdiff objects.

# forecast 2.04
  * Fixed bug in auto.arima() that occurred rarely.

# forecast 2.03
  * Added an option to auto.arima() to allow drift terms to be excluded from the models considered.

# forecast 2.02
  * Fixed bug in auto.arima() that occurred when there was an xreg but no drift, approximation=TRUE and stepwise=FALSE.

# forecast 2.01
  * Fixed bug in time index of croston() output.
  * Added further explanation about models to croston() help file.

# forecast 2.00
  * Package removed from forecasting bundle

# forecast 1.26 (29 August 2009)
  * Added as.data.frame.forecast(). This allows write.table() to work for forecast objects.

# forecast 1.25 (22 July 2009)
  * Added argument to auto.arima() and ndiffs() to allow the ADF test to be used instead of the KPSS test in selecting the number of differences.
  * Added argument to plot.forecast() to allow different colors and line types when plotting prediction intervals.
  * Modified forecast.ts() to give sensible results with a time series containing fewer than four observations.

# forecast 1.24 (9 April 2009)
  * Fixed bug in dm.test() to avoid errors when there are missing values in the residuals.
  * More informative error messages when auto.arima() fails to find a suitable model.

# forecast 1.23 (22 February 2009)
  * Fixed bugs that meant xreg terms in auto.arima() sometimes caused errors when stepwise=FALSE.

# forecast 1.22 (30 January 2009)
  * Fixed bug that meant regressor variables could not be used with seasonal time series in auto.arima().

# forecast 1.21 (16 December 2008)
  * Fixed bugs introduced in v1.20.

# forecast 1.20 (14 December 2008)
  * Updated auto.arima() to allow regression variables.
  * Fixed a bug in print.Arima() which caused problems when the data were inside a data.frame.
  * In forecast.Arima(), argument h is now set to the length of the xreg argument if it is not null.

# forecast 1.19 (7 November 2008)
  * Updated Arima() to allow regression variables when refitting an existing model to new data.

# forecast 1.18 (6 November 2008)
  * Bug fix in ets(): models with frequency less than 1 would cause R to hang.
  * Bug fix in ets(): models with frequency greater than 12 would not fit due to parameters being out of range.
  * Default lower and upper bounds on parameters ,  and  in ets() changed to 0.0001 and 0.9999 (instead of 0.01 and 0.99).

# forecast 1.17 (10 October 2008)
  * Calculation of BIC did not account for reduction in length of series due to differencing. Now fixed in auto.arima() and in print.Arima().
  * tsdiag() now works with ets objects.

# forecast 1.16 (29 September 2008)
  * Another bug fix in auto.arima(). Occasionally the root checking would cause an error. The condition is now trapped.

# forecast 1.15 (16 September 2008)
  * Bug fix in auto.arima(). The series wasn't always being stored as part of the return object when stepwise=FALSE.

# forecast 1.14 (1 August 2008)
  * The time series stored in M3 in the Mcomp package did not contain all the components listed in the help file. This problem has now been fixed.

# forecast 1.13 (16 June 2008)
  * Bug in plot.ets() fixed so that plots of non-seasonal models for seasonal data now work.
  * Warning added to ets() if the time series contains very large numbers (which can cause numerical problems). Anything up to 1,000,000 should be ok, but any larger and it is best to scale the series first.
  * Fixed problem in forecast.HoltWinters() where the lower and upper limits were interchanged.

# forecast 1.12 (22 April 2008)
  * Objects are now coerced to class ts in ets(). This allows it to work with zoo objects.
  * A new function dm.test() has been added. This implements the Diebold-Mariano test for predictive accuracy.
  * Yet more bug-fixes for auto.arima().

# forecast 1.11 (8 February 2008)
  * Modifications to auto.arima() in the case where ML estimation does not work for the chosen model. Previously this would return no model. Now it returns the model estimated using CSS.
  * AIC values reported in auto.arima() when trace=TRUE and approximation=TRUE are now comparable to the final AIC values.
  * Addition of the expsmooth package.

# forecast 1.10 (21 January 2008)
  * Fixed bug in seasadj() so it allows multiple seasonality
  * Fixed another bug in print.Arima()
  * Bug fixes in auto.arima(). It was sometimes returning a non-optimal model, and occasionally no model at all. Also, additional stationarity and invertibility testing is now done.

# forecast 1.09 (11 December 2007)
  * A new argument 'restrict' has been added to ets() with default TRUE. If set to FALSE, then the unstable ETS models are also allowed.
  * A bug in the print.Arima() function was fixed.

# forecast 1.08 (21 November 2007)
  * AICc and BIC corrected. Previously I had not taken account of the sigma^2 parameter when computing the number of parameters.
  * arima() function changed to Arima() to avoid the clash with the arima() function in the stats package.
  * auto.arima now uses an approximation to the likelihood when selecting a model if the series is more than 100 observations or the seasonal period is greater than 12. This behaviour can be over-ridden via the approximation argument.
  * A new function plot.ets() provides a decomposition plot of an ETS model.
  * predict() is now an alias for forecast() wherever there is not an existing predict() method.
  * The argument conf has been changed to level in all forecasting methods to be consistent with other R functions.
  * The functions gof() and forecasterrors() have been replaced by accuracy() which handles in-sample and out-of-sample forecast accuracy.
  * The initialization method used for a non-seasonal ETS model applied to seasonal data was changed slightly.
  * The following methods for ets objects were added: summary, coef and logLik.
  * The following methods for Arima objects were added: summary.

# forecast 1.07 (25 July 2007)
  * Bug fix in summary of in-sample errors. For ets models with multiplicative errors, the reported in-sample values of MSE, MAPE, MASE, etc., in summary() and gof() were incorrect.
  * ARIMA models with frequency greater than 49 now allowed. But there is no unit-root testing if the frequency is 50 or more, so be careful!
  * Improvements in documentation.

# forecast 1.06 (15 June 2007)
  * Bug fix in auto.arima(). It would not always respect the stated values of max.p, max.q, max.P and max.Q.
  * The tseries package is now installed automatically along with the forecasting bundle, whereas previously it was only suggested.

# forecast 1.05 (28 May 2007)
  * Introduced auto.arima() to provide a stepwise approach to ARIMA modelling. This is much faster than the old best.arima().
  * The old grid-search method used by best.arima() is still available by using stepwise=FALSE when calling auto.arima().
  * Automated choice of seasonal differences introduced in auto.arima().
  * Some small changes to the starting values of ets() models.
  * Fixed a bug in applying ets() to new data using a previously fitted model.

# forecast 1.04 (30 January 2007)
  * Added include.drift to arima()
  * Fixed bug in seasonal forecasting with ets()

# forecast 1.03 (20 October 2006)
  * Fixed some DOS line feed problems that were bothering unix users.

# forecast 1.02 (12 October 2006)
  * Added AICc option to ets() and best.arima().
  * Corrected bug in calculation of fitted values in ets models with multiplicative errors.

# forecast 1.01 (25 September 2006)
  * Modified ndiffs() so that the maximum number of differences allowed is 2.

# forecast 1.0 (31 August 2006)
  * Added MASE to gof().
  * croston() now returns fitted values and residuals.
  * arima() no longer allows linear trend + ARMA errors by default. Also, drift in non-stationary models can be turned off.
  * This version is the first to be uploaded to CRAN.

# forecast 0.99992 (8 August 2006)
  * Corrections to help files. No changes to functionality.

# forecast 0.99991 (2 August 2006)
  * More bug fixes. ets now converges to a good model more often.

# forecast 0.9999 (1 August 2006)
  * Mostly bug fixes.
  * A few data sets have been moved from fma to forecast as they are not used in my book.
  * ets is now considerably slower but gives better results. Full optimization is now the only option (which is what slows it down). I had too many problems with poor models when partial optimization was used. I'll work on speeding it up sometime, but this is not a high priority. It is fast enough for most use. If you really need to forecast 1000 series, run it overnight.
  * In ets, I've experimented with new starting conditions for optimization and it seems to be fairly robust now.
  * Multiplicative error models can no longer be applied to series containing zeros or negative values. However, the forecasts from these models are not constrained to be positive.

# forecast 0.999 (27 July 2006)
  * The package has been turned into three packages forming a bundle. The functions and a few datasets are still in the forecast package. The data from Makridakis, Wheelwright and Hyndman (1998) is now in the fma package. The M-competition data is now in the Mcomp package. Both fma and Mcomp automatically load forecast.
  * This is the first version available on all operating systems (not just Windows).
  * pegels has been replaced by ets. ets only fits the model; it doesn't produce forecasts. To get forecasts, apply the forecast function to the ets object.
  * ets has been completely rewritten which makes it slower, but much easier to maintain. Different boundary conditions are used and a different optimizer is used, so don't expect the results to be identical to what was done by the old pegels function. To get something like the results from the old pegels function, use forecast(ets()).
  * simulate.ets() added to simulate from an ets model.
  * Changed name of cars to auto to avoid clash with the cars data in the datasets package.
  * arima2 functionality is now handled by arima() and pegels2 functionality is now handled by ets.
  * best.arima now allows the option of BIC to be used for model selection.
  * Croston's method added in function croston().
  * ts.display renamed as tsdisplay
  * mean.f changed to meanf, theta.f changed to thetaf, rw.f changed to rwf, seasonaldummy.f to seasonaldummyf, sindex.f to sindexf, and spline.f to splinef. These changes are to avoid potential problems if anyone introduces an 'f' class.

# forecast 0.994 (4 October 2004)
  * Fixed bug in arima which caused predict() to sometimes fail when there was no xreg term.
  * More bug fixes in handling regression terms in arima models.
  * New print.Arima function for more informative output.

# forecast 0.993 (20 July 2004)
  * Added forecast function for structural time series models obtained using StructTS().
  * Changed default parameter space for pegels() to force admissibility.
  * Added option to pegels() to allow restriction to models with finite forecast variance. This restriction is imposed by default.
  * Fixed bug in arima.errors(). Changes made to arima() meant arima.errors() was often returning an error message.
  * Added a namespace to the package making fewer functions visible to the user.

# forecast 0.99 (21 May 2004)
  * Added automatic selection of order of differencing for best.arima.
  * Added possibility of linear trend in arima models.
  * In pegels(), option added to allow parameters of an exponential smoothing model to be in the 'admissible' (or invertible) region rather than within the usual (0,1) region.
  * Fixed some bugs in pegels.
  * Included all M1 and M3 data and some functions to subset and plot them.
  * Note: This package will only work in R1.9 or later.

# forecast 0.98 (23 August 2003)
  * Added facilities in pegels.
      o It is now possible to specify particular values of the smoothing parameters rather than always use the optimized values. If none are specified, the optimal values are still estimated as before.
      o It is also possible to specify upper and lower bounds for each parameter separately.
  * New function: theta.f. This implements the Theta method which did very well in the M3 competition.
  * A few minor problems with pegels fixed and a bug in forecast.plot that meant it didn't work when the series contained missing values.

# forecast 0.972 (11 July 2003)
  * Small bug fix: pegels did not return correct model when model was partially specified.

# forecast 0.971 (10 July 2003)
  * Minor fixes to make sure the package will work with R v1.6.x. No changes to functionality.

# forecast 0.97 (9 July 2003)
  * Fully automatic forecasting based on the state space approach to exponential smoothing has now been added. For technical details, see Hyndman, Koehler, Snyder and Grose (2002).
  * Local linear forecasting using cubic smoothing splines added. For technical details, see Hyndman, King, Pitrun and Billah (2002).

# forecast 0.96 (15 May 2003)
  * Many functions rewritten to make use of methods and classes. Consequently several functions have had their names changed and many arguments have been altered. Please see the help files for details.
  * Added functions forecast.Arima and forecat.ar
  * Added functions gof and seasadj
  * Fixed bug in plot.forecast. The starting date for the plot was sometimes incorrect.
  * Added residuals components to rw.f and mean.f.
  * Made several changes to ensure compatibility with Rv1.7.0.
  * Removed a work-around to fix a bug in monthplot command present in R v<=1.6.2.
  * Fixed the motel data set (columns were swapped)
