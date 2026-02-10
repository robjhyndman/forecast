# Changelog

## forecast 9.0.1

- Performance improvements for ARFIMA model search
- [`forecast.mlm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mlm.md)
  now finds `newdata` when passed as an argument from another function
  ([\#880](https://github.com/robjhyndman/forecast/issues/880))
- [`residuals.tslm()`](https://pkg.robjhyndman.com/forecast/reference/residuals.forecast.md)
  now allows `type = "working"` as per CRAN request
- Code modernization and performance improvements

## forecast 9.0.0

CRAN release: 2026-01-11

- [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) now
  allows missing values in the time series
  ([\#952](https://github.com/robjhyndman/forecast/issues/952))
- Added
  [`mean_model()`](https://pkg.robjhyndman.com/forecast/reference/mean_model.md)
  and
  [`forecast.mean_model()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md)
- Added
  [`rw_model()`](https://pkg.robjhyndman.com/forecast/reference/rw_model.md)
  and
  [`forecast.rw_model()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  (m-muecke,
  [\#969](https://github.com/robjhyndman/forecast/issues/969))
- Added
  [`spline_model()`](https://pkg.robjhyndman.com/forecast/reference/spline_model.md)
  and
  [`forecast.spline_model()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md)
  ([\#1013](https://github.com/robjhyndman/forecast/issues/1013))
- Added
  [`theta_model()`](https://pkg.robjhyndman.com/forecast/reference/theta_model.md)
  and
  [`forecast.theta_model()`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.md)
  ([\#1014](https://github.com/robjhyndman/forecast/issues/1014))
- Added
  [`croston_model()`](https://pkg.robjhyndman.com/forecast/reference/croston_model.md)
  and
  [`forecast.croston_model()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md)
  ([\#1015](https://github.com/robjhyndman/forecast/issues/1015))
- Added simulated and bootstrapped prediction intervals to more models
  ([\#1040](https://github.com/robjhyndman/forecast/issues/1040))
- Added parallelization for
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  (m-muecke,
  [\#346](https://github.com/robjhyndman/forecast/issues/346))
- More consistent handling of biasadj across models
- [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  rewritten to use S3 methods for models and remove `accuracy.default()`
  ([\#912](https://github.com/robjhyndman/forecast/issues/912))
- Bug fixes and performance improvements
- Documentation improvements

## forecast 8.24.0

CRAN release: 2025-04-08

- Documentation improvements
- Bug fixes

## forecast 8.23.0

CRAN release: 2024-06-20

- Prevented RNG state changing when the package is attached
  ([\#954](https://github.com/robjhyndman/forecast/issues/954),
  [\#955](https://github.com/robjhyndman/forecast/issues/955)).
- `head.ts()` and `tail.ts()` only defined for R \< 4.5.0 due to new
  base R functions.

## forecast 8.22.0

CRAN release: 2024-03-04

- `hfitted()` now much faster for ARIMA models (danigiro,
  [\#949](https://github.com/robjhyndman/forecast/issues/949))
- `hfitted()` now much faster for ETS models, and produces fitted values
  from initial states
  ([\#950](https://github.com/robjhyndman/forecast/issues/950))

## forecast 8.21.1

CRAN release: 2023-08-31

- [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  now allows p or P to be 0
- Bug fixes and improved docs

## forecast 8.21

CRAN release: 2023-02-27

- Fixed df calculation for Ljung-Box tests in
  [`checkresiduals()`](https://pkg.robjhyndman.com/forecast/reference/checkresiduals.md)
- Fixed some broken tests

## forecast 8.20

CRAN release: 2023-01-06

- Improvements to unit tests, and migrate to testthat 3e
- Prevent failure in C23 mode

## forecast 8.19

CRAN release: 2022-11-20

- Bug fixes

## forecast 8.18

CRAN release: 2022-10-02

- Updated RW forecasts to use an unbiased estimate of sigma2
- Bug fixes

## forecast 8.17.0

CRAN release: 2022-07-25

- Updated
  [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  to add alternative variance estimators.
  ([\#898](https://github.com/robjhyndman/forecast/issues/898))
- Added
  [`simulate.tbats()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  for simulating from TBATS models.
- Added dependency on generics for
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html) and
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  ([\#902](https://github.com/robjhyndman/forecast/issues/902))
- Bug fixes

## forecast 8.16

CRAN release: 2022-01-10

- Fixed
  [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md)
  incorrectly applying Box-Cox transformations when an `mts` is provided
  to the `data` argument
  ([\#886](https://github.com/robjhyndman/forecast/issues/886)).
- Set D=0 when
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  applied to series with 2m observations or fewer.
- Improved performance of parallel search of ARIMA models (jonlachmann,
  [\#891](https://github.com/robjhyndman/forecast/issues/891)).
- Fixed scoping of functions used in
  [`ggAcf()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.acf.md)
  ([\#896](https://github.com/robjhyndman/forecast/issues/896)).
- Fixed checks on xreg in
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  ([\#818](https://github.com/robjhyndman/forecast/issues/818))
- Improved docs and bug fixes.

## forecast 8.15

CRAN release: 2021-06-01

- Changed [`summary()`](https://rdrr.io/r/base/summary.html) methods to
  defer console output until
  [`print()`](https://rdrr.io/r/base/print.html)
- Changed default `s.window` values for
  [`mstl()`](https://pkg.robjhyndman.com/forecast/reference/mstl.md),
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  and
  [`stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.md).
  The new defaults are based on extensive empirical testing.

## forecast 8.14

CRAN release: 2021-03-11

- Changed default `BoxCox(lambda = "auto")` lower bound to -0.9.
- Use better variance estimates for ets bias adjustments.
- Improved robustness of
  [`autoplot.seas()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.seas.md)
  for non-seasonal decomposition.
- Fixed scoping of parameters in `auto.arima(parallel = TRUE)`
  ([\#874](https://github.com/robjhyndman/forecast/issues/874)).
- Fixed handling of `xreg` in
  [`tsCV()`](https://pkg.robjhyndman.com/forecast/reference/tsCV.md).

## forecast 8.13

CRAN release: 2020-09-12

- Fixed forecasts from
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  with drift with initial NAs.
- Fixed season colours in
  [`gglagplot()`](https://pkg.robjhyndman.com/forecast/reference/gglagplot.md)
  to match y-axis (original data).
- Fixed facet order for classical decomposition
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
- Fixed [`summary()`](https://rdrr.io/r/base/summary.html) erroring for
  [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md)
  models containing NA values.

## forecast 8.12

CRAN release: 2020-03-31

- Fixed bias adjusted forecast mean for ARIMA forecasts.
- Improved naming of
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  generic formals.
- Fix seasonal periods for `taylor` dataset.

## forecast 8.11

CRAN release: 2020-02-09

- The axis for
  [`gglagplot()`](https://pkg.robjhyndman.com/forecast/reference/gglagplot.md)
  have been reversed for consistency with
  [`stats::lag.plot()`](https://rdrr.io/r/stats/lag.plot.html).

## forecast 8.10

CRAN release: 2019-12-05

- Updates to remove new CRAN errors
- Bug fixes

## forecast 8.9

CRAN release: 2019-08-22

- Updates for CRAN policies on Suggests packages
- Bug fixes

## forecast 8.8

CRAN release: 2019-08-02

- Updates for compatibility with fable
- Bug fixes

## forecast 8.7

CRAN release: 2019-04-29

- Documentation improvements
- Bug fixes

## forecast 8.6

CRAN release: 2019-04-16

- Reduced conflicts with tidy forecasting packages
- Forecast autoplots now use same colour shading as
  [`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html)
  and geom_forecast
- Documentation improvements
- Bug fixes

## forecast 8.5

CRAN release: 2019-01-18

- Updated
  [`tsCV()`](https://pkg.robjhyndman.com/forecast/reference/tsCV.md) to
  handle exogenous regressors
- Reimplemented lagwalk methods
  ([`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
  [`snaive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
  [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md))
  for speed improvements
- Added support for passing arguments to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  unit root tests
- Improved
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  stepwise search algorithm
- Documentation improvements
- Bug fixes

## forecast 8.4

CRAN release: 2018-06-21

- Added
  [`modelAR()`](https://pkg.robjhyndman.com/forecast/reference/modelAR.md),
  generalising
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  to support user-defined functions
- Added na.action argument to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
- Documentation improvements
- Bug fixes

## forecast 8.3

CRAN release: 2018-04-11

- Added
  [`mstl()`](https://pkg.robjhyndman.com/forecast/reference/mstl.md) to
  handle multiple seasonal decomposition
- [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md),
  [`stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.md),
  [`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md)
  and
  [`tsclean()`](https://pkg.robjhyndman.com/forecast/reference/tsclean.md)
  all now use
  [`mstl()`](https://pkg.robjhyndman.com/forecast/reference/mstl.md).
- Updated
  [`tsCV()`](https://pkg.robjhyndman.com/forecast/reference/tsCV.md) to
  handle multiple horizons
- Switched unit root tests in
  [`ndiffs()`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.md)
  to use urca package
- Added
  [`ocsb.test()`](https://pkg.robjhyndman.com/forecast/reference/ocsb.test.md)
- Changed method for choosing D in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  to a measure of seasonal strength.
- Added
  [`baggedModel()`](https://pkg.robjhyndman.com/forecast/reference/baggedModel.md)
  function to generalize baggedETS
- Added bootstrapped PI to more functions
- Allowed lambda=‘auto’ for all functions with lambda argument.
- Updated author list to include all major contributors
- Documentation improvements
- Bug fixes

## forecast 8.2

CRAN release: 2017-09-25

- Added pkgdown site
- Added rolling window option to
  [`tsCV()`](https://pkg.robjhyndman.com/forecast/reference/tsCV.md)
- Improved robustness to short time series and missing values
- Bug fixes

## forecast 8.1

CRAN release: 2017-06-17

- Added
  [`as.character.ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md),
  [`as.character.bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md),
  [`as.character.tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
- Made
  [`gghistogram()`](https://pkg.robjhyndman.com/forecast/reference/gghistogram.md)
  and
  [`checkresiduals()`](https://pkg.robjhyndman.com/forecast/reference/checkresiduals.md)
  robust to missing values
- All documentation now generated using roxygen
- Improved documentation for many functions
- Added
  [`autoplot.msts()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
  and
  [`autolayer.msts()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
- Added as.character methods for many models to generate model names
- Added
  [`as.ts.forecast()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
- autoplot method for bats/tbats models
- Better ARIMA trace output
- Made accuracy an S3 method
- Bug fixes

## forecast 8.0

CRAN release: 2017-02-23

- Added tips to start up message
- Added pipe operator
- Added
  [`tsCV()`](https://pkg.robjhyndman.com/forecast/reference/tsCV.md) and
  [`CVar()`](https://pkg.robjhyndman.com/forecast/reference/CVar.md)
  functions
- Added baggedETS
- Added `head.ts()` and `tail.ts()`, so head and tail now work properly
  on ts objects.
- Added
  [`gghistogram()`](https://pkg.robjhyndman.com/forecast/reference/gghistogram.md)
  and
  [`checkresiduals()`](https://pkg.robjhyndman.com/forecast/reference/checkresiduals.md)
- Added
  [`ggseasonplot()`](https://pkg.robjhyndman.com/forecast/reference/seasonplot.md)
  with polar coordinates
- Modified defaults for
  [`gglagplot()`](https://pkg.robjhyndman.com/forecast/reference/gglagplot.md)
- Added
  [`autolayer.ts()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
- Added type argument to
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) for different
  types of residuals
- Added support for seas objects from the seasonal package
- Component extraction for seasonal decomposition methods
- Range bars for decomposition autoplots
- Added
  [`autoplot.StructTS()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.seas.md)
- Added vignette based on 2008 JSS article by Hyndman and Khandakar
- Improved ggplot functions
- mforecast objects re-structured
- Added
  [`as.data.frame.mforecast()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mts.md)
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  functions now exported
- Refit support for
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  and [`stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.md)
- Better bias adjustment support after Box-Cox transformation
- [`print.ARIMA()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  has better labelling of constants
- Bug fixes
- Removed fortify method for forecast objects

## forecast 7.3

CRAN release: 2016-10-13

- Added prediction intervals and simulation for
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md).
- Documentation improvement
- Bug fixes

## forecast 7.2

CRAN release: 2016-09-09

- Faceting for
  [`autoplot.mts()`](https://pkg.robjhyndman.com/forecast/reference/autoplot.ts.md)
- Box-Cox support for ses, holt, hw
- [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) now
  works for tiny time series
- Added h-step fitted values in
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) function.
- seasonal adjustment added to
  [`thetaf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.md)
- y now the standard first argument in all modelling functions
- Added truncate argument to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
- [`seasadj()`](https://pkg.robjhyndman.com/forecast/reference/seasadj.md)
  now an S3 method
- series with frequency \< 1 and non-integer seasonality now handled
  better
- ggplot2 theme support
- Added gglagplot, gglagchull
- [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  and
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  now allow any argument to be passed to
  [`stats::arima()`](https://rdrr.io/r/stats/arima.html).
- Bug fixes and speed improvements

## forecast 7.1

CRAN release: 2016-04-14

- Fixed bug in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  where the Box-Cox transformation was sometimes applied twice
- Improved axes for ggseasonalplot
- Improved
  [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md) to
  avoid some problems finding data
- [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  updated to allow subsets
- Modified initial values for
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
- Improved unit tests to avoid deprecated functions and to avoid data
  from fpp
- Removed fpp from Suggests list

## forecast 7.0

CRAN release: 2016-04-04

- Added ggplot2 graphics
- Bias adjustment option added for all functions that allow Box-Cox
  transformations
- Added [`Ccf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md)
  function, and rewrote
  [`Acf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md) to
  handle multivariate series.
- [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md)
  completely rewritten to be more robust and to handle fourier terms
  more easily
- Support for multivariate linear models added
- [`subset.ts()`](https://pkg.robjhyndman.com/forecast/reference/subset.ts.md)
  more robust, and captures some errors.
- Added xreg argument to
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
- Improved labels in seasonplot
- More unit tests added
- Documentation improvements
- Bug fixes

## forecast 6.2

CRAN release: 2015-10-20

- Many unit tests added using testthat.
- Fixed bug in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) when
  very short seasonal series were passed in a data frame.
- Fixed bug in
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  where the initial predictor vector was reversed.
- Corrected model name returned in
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md).
- Fixed bug in
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  when non-integer seasonality used.
- Made
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  robust to non-integer seasonality.
- Fixed bug in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  where allowmean was ignored when stepwise=FALSE.
- Improved robustness of
  [`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md)
  for explosive models with multiplicative trends.
- Exogenous variables now passed to VAR forecasts
- Increased maximum nmse in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) to
  30.
- Made
  [`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md)
  more robust to weak seasonality
- Changed
  [`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md)
  to use supsmu on non-seasonal and seasonally adjusted data.
- Fixed bug in
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
  when seasonal period 1 is a small multiple of seasonal period 2.
- Other bug fixes

## forecast 6.1

CRAN release: 2015-05-12

- Made
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  more robust

## forecast 6.0

CRAN release: 2015-05-09

- Modified
  [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  to give error when variance is zero
- Corrected help file for
  [`splinef()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md).
- Fixed typo in accuracy help file regarding RMSE
- Fixed bug in
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  which occurred with Arima and ets objects.
- Fixed
  [`arima.errors()`](https://pkg.robjhyndman.com/forecast/reference/arima.errors.md)
  to handle Box-Cox transformed models.
- Modified
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  to be stricter on near-unit-roots.
- Added allowmean argument in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Improved handling of constant series in
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  and
  [`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md).
- Added
  [`plot.Arima()`](https://pkg.robjhyndman.com/forecast/reference/plot.Arima.md)
  and
  [`plot.ar()`](https://pkg.robjhyndman.com/forecast/reference/plot.Arima.md)
  functions.
- Added as.character.Arima
- Captured problem in bats/tbats where data are constant.
- Modified TBATS and BATS estimation to avoid occasional instabilities.
- Fixed bug in forecasts from bats which labelled them as TBATS.
- Added allow.multiplicative.trend argument to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).
- Set allow.multiplictive.trend=FALSE in
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md),
  [`stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.md) and
  [`forecast.ts()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md).
- Simplified arguments in
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).
- Added taperedacf and taperedpacf functions
- Added functions for bootstrapping time series

## forecast 5.9

CRAN release: 2015-02-26

- Improved documentation of
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  function.
- Fixed occasional bug in
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  when test set is a single observation.
- Improved
  [`Acf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md) to
  give better handling of horizontal axis for seasonal data or when … is
  passed.
- Removed `print.Arima()` and `predict.Arima()` and added
  [`print.ARIMA()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
- method argument now passed when re-fitting an ARIMA model.
- Fixed error when CH test applied to short series

## forecast 5.8

CRAN release: 2015-01-06

- Fixed bug in versions of R before 3.10 when using fourier and
  fourierf.
- Made
  [`BoxCox.lambda()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.lambda.md)
  robust to missing values.

## forecast 5.7

CRAN release: 2014-12-17

- Fixed bug in tbats/bats where optional arguments were not being passed
  to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Revised
  [`fourier()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)
  and
  [`fourierf()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)
  to avoid large orders, and to avoid zero columns.
- Improved accuracy of
  [`fourier()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)
  and
  [`fourierf()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md),
  while simplifying the code.
- Removed duplicate columns returned by fourier/fourierf with multiple
  seasonal periods.
- Corrected some bugs in
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  for models involving xreg.
- Centred simulations from
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  for non-stationary models by conditioning on first observation.
- Added
  [`findfrequency()`](https://pkg.robjhyndman.com/forecast/reference/findfrequency.md)
  function.
- Fixed error in computed residuals from
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).
- Improved handling of very short series in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Fixed error in forecasting with additive damped models. Damping
  previously applied only from second forecast horizon.
- Fixed misuse of [`abs()`](https://rdrr.io/r/base/MathFun.html) in two
  places in C code.
- Added na.action argument to
  [`Acf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md) and
  fixed na.action argument in
  [`tsdisplay()`](https://pkg.robjhyndman.com/forecast/reference/tsdisplay.md).

## forecast 5.6

CRAN release: 2014-09-24

- Improved tbats and bats by ensuring ARMA coefficients are not close to
  the boundary of invertibility and stationarity.
- Improved
  [`nsdiffs()`](https://pkg.robjhyndman.com/forecast/reference/nsdiffs.md)
  handling of degenerate series (e.g., all zeros).
- Improved
  [`forecast.ar()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  when function buried within other functions.
- Improved handling of degenerate ARIMA models when xreg used.
- More robust ets initialization.
- Fixed problem in
  [`na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.md)
  with seasonal data having frequency \<= 5.
- Removed undocumented option to use Rmalschains for optimization of
  ets.

## forecast 5.5

CRAN release: 2014-08-12

- Improved documentation for croston
- Added
  [`stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.md) and
  [`forecast.stlm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  functions, and added forecastfunction argument as a way of specifying
  a forecast method in
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  and
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).
- Improved
  [`forecast.ar()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  so that it is more likely to work if
  [`ar()`](https://rdrr.io/r/stats/ar.html) and
  [`forecast.ar()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  are embedded within other functions.
- Improved handling of ARIMA models with seasonality greater than 48
- Improved handling of some degenerate regression models in nsdiffs
- Changed AIC for poor models from 1e20 to Inf.
  - Update
    [`fourier()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)
    and
    [`fourierf()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md)
    to work with msts object.
  - Added a new argument find.frequency to
    [`forecast.ts()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md).
  - Added new arguments d and D to
    [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
    for MASE.
- Corrected bugs in
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html).
- Better handling of regression models with perfect fit in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Fixed bug in
  [`tbats.components()`](https://pkg.robjhyndman.com/forecast/reference/tbats.components.md)
  when there are no seasonal components.

## forecast 5.4

CRAN release: 2014-05-08

- Fixed bug in
  [`forecast.tbats()`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md)
  and
  [`forecast.bats()`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md)
  when ts.frequency does not match seasonal.periods.
- Fixed bug in
  [`getResponse.lm()`](https://pkg.robjhyndman.com/forecast/reference/getResponse.md)
  when there’s a logged dependent variable.
- Modified
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) to
  avoid problems when data contains large numbers.
- Modified
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) to
  produce forecasts when the data are constant.
- Improved
  [`arima.errors()`](https://pkg.robjhyndman.com/forecast/reference/arima.errors.md)
  to find xreg more often, and to return an error if it can’t be found.

## forecast 5.3

CRAN release: 2014-03-24

- Unit tests added
- Fixed bug in `zzhw()` which reversed the sign of the residuals.
- Updated help file for
  [`CV()`](https://pkg.robjhyndman.com/forecast/reference/CV.md) to
  specify it is only leave-one-out.
- Fixed `guer.cv()` to allow non-integer periods without warning.
- Added use.initial.values argument in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).
- Added
  [`arimaorder()`](https://pkg.robjhyndman.com/forecast/reference/arimaorder.md)
  function.
- Modified warnings suppression by using
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html)
  throughout.

## forecast 5.2

CRAN release: 2014-02-24

- Changed default number of cores to 2 for all functions that use
  parallel processing.
- Removed remaining call to
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md)
  from examples that are run.

## forecast 5.1

CRAN release: 2014-02-08

- Fixed bug in
  [`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md)
  and
  [`tsclean()`](https://pkg.robjhyndman.com/forecast/reference/tsclean.md)
  with very short seasonal series.
- Fixed bug in
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  when seasonal order is specified numerically instead of via a list.
- Removed dimension attribution from output of
  [`arima.errors()`](https://pkg.robjhyndman.com/forecast/reference/arima.errors.md)
- Improved handling of “test” in accuracy
- Changed parallel processing to parLapply for
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
- Added timeDate dependency to avoid errors in
  [`easter()`](https://pkg.robjhyndman.com/forecast/reference/easter.md)
  and link to Rcpp \>= 0.11.0.

## forecast 5.0

CRAN release: 2014-01-17

- Added argument model to
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md).
- Added
  [`bizdays()`](https://pkg.robjhyndman.com/forecast/reference/bizdays.md)
  and
  [`easter()`](https://pkg.robjhyndman.com/forecast/reference/easter.md)
  for calendar variables.
- Added arguments max.D and max.d to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md),
  [`ndiffs()`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.md)
  and
  [`nsdiffs()`](https://pkg.robjhyndman.com/forecast/reference/nsdiffs.md).
- Made several functions more robust to zoo objects.
- Corrected an error in the calculation of AICc when using
  [`CV()`](https://pkg.robjhyndman.com/forecast/reference/CV.md).
- Made minimum default p in
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  equal to 1.
- Added
  [`tsoutliers()`](https://pkg.robjhyndman.com/forecast/reference/tsoutliers.md)
  and
  [`tsclean()`](https://pkg.robjhyndman.com/forecast/reference/tsclean.md)
  for identifying and replacing outliers
- Improved
  [`na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.md)
  to handle seasonality and added argument lambda to
  [`na.interp()`](https://pkg.robjhyndman.com/forecast/reference/na.interp.md)
- Added robust option to
  [`forecast.ts()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
  to allow outliers and missing values
- Improved output from
  [`snaive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  and
  [`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  to better reflect user expectations
- Allowed
  [`Acf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md) to
  handle missing values by using na.contiguous
- Changed default information criterion in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) to
  AICc.
- Removed drift term in
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  when d+D\>1.
- Added bootstrap option to
  [`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)

## forecast 4.8

CRAN release: 2013-09-30

- Fixed bug in
  [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  that was introduced in v4.7

## forecast 4.7

CRAN release: 2013-09-27

- Added `forecast.forecast()` to simply return the object that is
  passed.
- Removed leading zero in package number. i.e., 4.7 instead of 4.07.
- better handling of nearly constant time series, and nearly linear time
  series
- improved handling of missing values in
  [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
- corrected fitted values and residuals in
  [`meanf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md)
  for time series data
- [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) and
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
  now handle missing values in the same way as
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).
  i.e., using longest contiguous portion.
- better handling of very short time series
- initial states for
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
  modified for very short time series (less than 3 years).
- nsdiffs with CH test now handles degenerate cases without returning an
  error.
- nnetar now handles missing values
- Fixed bug in `forecast.varest()` so residuals and fitted values
  computed correctly.
- Added
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  calculation for VAR models
- Fixed a bug in
  [`simulate.fracdiff()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  when future=TRUE. Sometimes the future argument was being ignored.

## forecast 4.06

CRAN release: 2013-06-30

- [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html) was
  returning a mape and mpe 100 times too large for in-sample errors.

## forecast 4.05

CRAN release: 2013-06-19

- Fixed bug in
  [`hw()`](https://pkg.robjhyndman.com/forecast/reference/ses.md) so it
  works when initial=“simple”
- Allowed
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) and
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
  to take non-positive values.
- [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) now
  calls optim direct via c code making
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) run
  much faster.
- Added Rmalschains as a possible optimizer in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md). Not
  documented.
- Modified
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  so it is more likely that the original data are stored in the returned
  object.
- Corrected bug in
  [`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  that occurred when a Box-Cox transformation was used with
  bootstrap=TRUE.
- [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  updated so that it gives more information, and returns a matrix of
  both test and training measures.
- Corrected training error measures for
  [`splinef()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md)
  forecasts.

## forecast 4.04

CRAN release: 2013-04-22

- Added ylim argument to
  [`Acf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md)
- Avoided clash with the signal package when using
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Fixed problem in
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  when all historical data are NA or when there is no available
  historical data.
- [`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  is now a little more robust if a zoo object is passed instead of a ts
  object.
- [`CV()`](https://pkg.robjhyndman.com/forecast/reference/CV.md) now
  handles missing values in the residuals.
- Fixed bug in
  [`holt()`](https://pkg.robjhyndman.com/forecast/reference/ses.md) and
  [`hw()`](https://pkg.robjhyndman.com/forecast/reference/ses.md) so
  that the printed model no longer contains missing values.

## forecast 4.03

CRAN release: 2013-03-17

- [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  now guesses the variable name if there is only one predictor variable.
- Removed error trap in
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  when no xreg variables passed as it was catching legitimate calls.

## forecast 4.02

CRAN release: 2013-03-06

- Fixed error in the prediction intervals returned by
  [`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md)
  when simulation was used and a Box-Cox transformation was specified.
- Fixed bug in
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  when a numerical f vector was passed.
- Fixed man file for Diebold-Mariano test.
- Corrected references in
  [`nsdiffs()`](https://pkg.robjhyndman.com/forecast/reference/nsdiffs.md)
  help page.
- Added warning to nsdiffs when series too short for seasonal
  differencing.
- Fixed problem in getResponse.Arima when Arima object created by
  stats::[`arima()`](https://rdrr.io/r/stats/arima.html) from within a
  function.
- Added
  [`tbats.components()`](https://pkg.robjhyndman.com/forecast/reference/tbats.components.md)
  and extended
  [`seasadj()`](https://pkg.robjhyndman.com/forecast/reference/seasadj.md)
  to allow tbats objects.
- Added undocumented functions for forecasting, printing and plotting
  output from vars::VAR.

## forecast 4.01

CRAN release: 2013-01-22

- Error now trapped when newxreg variables not passed to
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
- Corrected help file for
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md) to
  remove references to prediction intervals.
- Improved help file for
  [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  to give more information about the alternative hypotheses.
- Improved
  [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  performance for small samples by using a t-distribution instead of
  normal.
- Modified
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) and
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
  examples to follow CRAN policies on parallel processing.
- Moved some packages from Depends to Imports.
- Added
  [`getResponse()`](https://pkg.robjhyndman.com/forecast/reference/getResponse.md)
  function to return the historical time series from various time series
  model objects.
- Modified
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html) to
  use
  [`getResponse()`](https://pkg.robjhyndman.com/forecast/reference/getResponse.md).
- Allowed user-generated innovations in
  [`simulate.ets()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md),
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md),
  etc.
- Allowed xreg argument in
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  and
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  when ARIMA model used.
- Removed reliance on caret, and associated fitted and residuals
  functions.

## forecast 4.00

CRAN release: 2012-11-27

- More robust handling of degenerate ARIMA models.
- New defaults for shaded colors used for prediction intervals in plots.
- [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  now remembers the name of the series when a Box-Cox transformation is
  used.
- New function
  [`nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.md)
  for automatic neural network forecasting of time series.
- [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  now tries harder to ensure the ARMA part is stationary.
- ts control added for forecast of linear models in
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md).
- Fixed bug in
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md)
  which caused an error when use.box.cox=FALSE and use.trend=FALSE.
- Added residuals and fitted methods for train and avNNet objects from
  caret package.
- [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html) can
  now figure out overlapping times for x and f.
- [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  now handles missing values.
- Revised
  [`ses()`](https://pkg.robjhyndman.com/forecast/reference/ses.md),
  [`holt()`](https://pkg.robjhyndman.com/forecast/reference/ses.md) and
  [`hw()`](https://pkg.robjhyndman.com/forecast/reference/ses.md) so
  that they can optionally use traditional initialization.

## forecast 3.25

CRAN release: 2012-09-11

- Fixed bug in
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md).
- Improved handling of short seasonal time series in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Added seasonal argument to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Fixed bug in
  [`splinef()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md)
  and added gcv method for estimating smoothing parameter.

## forecast 3.24 (23 July 2012

CRAN release: 2012-07-23

- Fixed bug in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  introduced in v3.23 which meant a ARIMA(0,0,0) model was returned
  about half the time.

## forecast 3.23

CRAN release: 2012-07-18

- Fixed bug in
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  which meant the drange argument was being ignored.
- Extended
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  so it returns something sensible when the data are constant.

## forecast 3.22

CRAN release: 2012-06-07

- Increased maximum forecast horizon for ets models from 2000 to
  unlimited.
- Corrected bug in
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md).
  Previously include.constant=FALSE was ignored.
- Some corrections to bats and tbats.
- Modified parallel implementation in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  for Windows.

## forecast 3.21

CRAN release: 2012-04-30

- Fixed bug in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  when lambda is non-zero and stepwise is FALSE.
- Fixed bug in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  in selecting d when D\>0.
- Fixed bug in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) when
  seasonal period is less than 1.
- Turned off warnings in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  and [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
  when seasonal period is less than 1.
- Added plotting methods for bats and tbats objects.
- Changed default forecast horizons for bats and tbats objects.
- Modified bats and tbats so they now use seasonal.periods when ts and
  msts objects are being modelled.

## forecast 3.20

CRAN release: 2012-04-02

- Fixed bugs in
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md).
- Improved handling of newdata in
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  to provide more meaningful error messages.
- Fixed bug in
  [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  that occurred when errors were very small.

## forecast 3.19

CRAN release: 2012-02-22

- Improved plotting of forecast objects from lm models
- Added MASE for lm forecasts using insample mean forecasts for scaling.
- Modified definition of MASE for seasonal time series to use seasonal
  [`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  insample scaling.
- Modified
  [`meanf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md)
  to allow it to be used with cross-sectional data.
- Updated
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html) to
  allow it to be used with cross-sectional data, lm forecasts and lm
  objects.

## forecast 3.18

CRAN release: 2012-02-17

- Added method for plotting non-time-series forecasts to
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md).
- Removed partial arg matching.
- Cleaned up some code, removing commented out sections, etc.
- Added robust option to
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).
- Added
  [`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  and rwdrift options to
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  and
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).
- Improved handling of msts objects in
  [`BoxCox.lambda()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.lambda.md)
- Fixed some minor bugs in
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
  and bats
- Improved speed of
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) and
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md).

## forecast 3.17

CRAN release: 2012-02-02

- Improved
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  so it is more likely to find the original data from an lm object.
- Parallel processing now available in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  when stepwise=FALSE
- Default model selection in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  changed to AICc rather than AIC. This may affect model selection for
  very short time series.
- max orders in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  now restricted to be less than 1/3 of length of data.

## forecast 3.16

CRAN release: 2011-12-24

- Corrected problem with AIC computation in bats and tbats
- Fixed handling of non-seasonal data in bats
- Changed dependency to \>= R 2.14.0 in order to ensure parallel package
  available.

## forecast 3.15

CRAN release: 2011-12-22

- New functions
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
  and
  [`forecast.tbats()`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md)
  for multiple seasonal time series modelling.
- [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) and
  [`tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.md)
  use parallel processing when possible.
- Minor improvements to
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md) and
  [`forecast.bats()`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md).
- [`decompose()`](https://rdrr.io/r/stats/decompose.html) removed as the
  function in the stats package has now been fixed.

## forecast 3.14

CRAN release: 2011-12-09

- Improved documentation for
  [`forecast.ts()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
- Corrected bug in
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md)
  when applied to a non-ts object.
- Added error message when
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md)
  applied to data containing zeros or negative values
- Added checks when
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md)
  applied to time series with non-nested periods.
- Added msts object class for multiple seasonal time series
- Made taylor data set an msts object.
- Added
  [`bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.md)
  function for multiple seasonal time series modelling
- Added
  [`forecast.bats()`](https://pkg.robjhyndman.com/forecast/reference/forecast.bats.md)
  function for forecasting BATS models
- Byte compiling turned on
- Depending on Rcpp and RcppArmadillo to speed some code up.

## forecast 3.13

CRAN release: 2011-11-20

- Bug fix for
  [`forecast.StructTS()`](https://pkg.robjhyndman.com/forecast/reference/forecast.StructTS.md)
  due to changes in the StructTS object. The default h was being set
  to 0. Thanks to Tarmo Leinonen for reporting this problem.
- Bug fix for
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  where h longer than one seasonal period sometimes returned missing
  forecasts. Thanks to Kevin Burton for reporting this problem.
- [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  no longer allows a seasonal ETS model to be specified. Thanks to
  Stefano Birmani for the suggestion.

## forecast 3.12

CRAN release: 2011-11-16

- Added option to control ets model in
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  and
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).
  Thanks to Stefano Birmani for the suggestion.
- Reordered arguments for
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  and
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  to be consistent with other forecast functions.
- Modified
  [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md) so
  that it is more likely to find the relevant data when it is not passed
  as an argument.
- Fixed bug in
  [`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md)
  which returned all zero forecasts for some models when seasonal period
  \> 24.

## forecast 3.11

CRAN release: 2011-11-02

- Fixed bug in
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md)
  when smallest period is odd

## forecast 3.10

CRAN release: 2011-10-27

- Added lambda argument to
  [`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  and
  [`snaive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md).
- Fixed bug in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) with
  high frequency data.
- Fixed bug in
  [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  where incorrect fitted values and residuals were sometimes returned.
- Modified number of lags displayed by default in
  [`tsdisplay()`](https://pkg.robjhyndman.com/forecast/reference/tsdisplay.md).

## forecast 3.09

CRAN release: 2011-10-18

- Fixed bug causing occasional problems in
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  when MA order greater than 2 and future=TRUE.

## forecast 3.08

CRAN release: 2011-10-15

- Bug fix in
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  which occurred when forecast horizon is less than seasonal period.
- Added lambda argument to
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).

## forecast 3.07

CRAN release: 2011-10-11

- Bug fix in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
  concerning non-seasonal models and high-frequency data. It sometimes
  returned all forecasts equal to zero.

## forecast 3.06

CRAN release: 2011-10-04

- Switched to useDynLib in preparation for Rv2.14.0.

## forecast 3.05

CRAN release: 2011-10-03

- Fixed bug in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) which
  prevent non-seasonal models being fitted to high frequency data.

## forecast 3.04

CRAN release: 2011-09-23

- Fixed bug when drift and xreg used together in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  or
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md).

## forecast 3.03

CRAN release: 2011-09-02

- Bug fix in
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md)
  which was using slightly incorrect seasonal estimates for the
  forecasts
- Bug fix in
  [`forecast.StructTS()`](https://pkg.robjhyndman.com/forecast/reference/forecast.StructTS.md)
  due to change in structure of StructTS object.
- Better error capture in tslm when seasonal dummies are specified for
  non-seasonal data.
- Re-formatted some help files to prevent viewing problems with the pdf
  manual.

## forecast 3.02

CRAN release: 2011-08-25

- Bug fixes

## forecast 3.00

CRAN release: 2011-08-24

- Added Box-Cox parameter as argument to
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md),
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md),
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md),
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md),
  [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
  [`meanf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md),
  [`splinef()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md)
- Added Box-Cox parameter as argument to
  [`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md),
  [`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md),
  [`forecast.fracdiff()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md),
  [`forecast.ar()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md),
  [`forecast.StructTS()`](https://pkg.robjhyndman.com/forecast/reference/forecast.StructTS.md),
  [`forecast.HoltWinters()`](https://pkg.robjhyndman.com/forecast/reference/forecast.HoltWinters.md).
- Removed lambda argument from
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  and
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html).
- Added
  [`BoxCox.lambda()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.lambda.md)
  function to allow automatic choice for Box-Cox parameter using
  Guerrero’s method or the profile log likelihood method.
- Modified BoxCox and InvBoxCox to return missing values when lambda \<
  0 and data \< 0.
- Add
  [`nsdiffs()`](https://pkg.robjhyndman.com/forecast/reference/nsdiffs.md)
  function for selecting the number of seasonal differences.
- Modified selection of seasonal differencing in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Better error message if seasonal factor used in
  [`tslm()`](https://pkg.robjhyndman.com/forecast/reference/tslm.md)
  with non-seasonal data.
- Added PI argument to
  [`forecast.ets()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ets.md)
  to allow only point forecasts to be computed.
- Added include.constant argument to
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md).
- Added
  [`subset.ts()`](https://pkg.robjhyndman.com/forecast/reference/subset.ts.md)
  function.
- Upgraded
  [`seasonplot()`](https://pkg.robjhyndman.com/forecast/reference/seasonplot.md)
  function to allow colors and to fix some bugs.
- Fixed fitted values returned by
  [`forecast.HoltWinters()`](https://pkg.robjhyndman.com/forecast/reference/forecast.HoltWinters.md)
- Modified
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  because of undocumented changes in
  [`filter()`](https://rdrr.io/r/stats/filter.html) function in stats
  package.
- Changed residuals returned by
  [`splinef()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md)
  to be ordinary residuals. The standardized residuals are now returned
  as standardizedresiduals.
- Added
  [`dshw()`](https://pkg.robjhyndman.com/forecast/reference/dshw.md)
  function for double-seasonal Holt-Winters method based on Taylor
  (2003).
- Fixed further bugs in the
  [`decompose()`](https://rdrr.io/r/stats/decompose.html) function that
  caused the results to be incorrect with odd frequencies.

## forecast 2.19

CRAN release: 2011-06-04

- Added xreg information to the object returned by
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Added
  [`Acf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md),
  [`Pacf()`](https://pkg.robjhyndman.com/forecast/reference/Acf.md),
  [`ma()`](https://pkg.robjhyndman.com/forecast/reference/ma.md) and
  [`CV()`](https://pkg.robjhyndman.com/forecast/reference/CV.md)
  functions.
- Fixed bugs in re-fitting ARIMA models to new data.

## forecast 2.18 (2011-05-19)

- Fixed bug in
  [`seasonplot()`](https://pkg.robjhyndman.com/forecast/reference/seasonplot.md)
  where year labels were sometimes incorrect.

## forecast 2.17

CRAN release: 2011-04-06

- Modified
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  to handle seasonal ARIMA models.
- Modified
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) to
  handle missing values. The largest continuous section of data is now
  modelled.
- Improved
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  to handle missing values at the end of the observed series.
- Added replacement
  [`decompose()`](https://rdrr.io/r/stats/decompose.html) to avoid
  truncation of seasonal term and seasonally adjusted series.
- Fixed bug in
  [`seasadj()`](https://pkg.robjhyndman.com/forecast/reference/seasadj.md)
  to handle multiplicative decomposition, and to avoid missing values at
  ends.

## forecast 2.16

CRAN release: 2011-03-07

- Changed the way missing values are handled in tslm

## forecast 2.15

CRAN release: 2011-03-05

- Added
  [`fourier()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md),
  [`fourierf()`](https://pkg.robjhyndman.com/forecast/reference/fourier.md),
  tslm
- Improved
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
  to allow trend and seasonal terms.

## forecast 2.14

CRAN release: 2011-03-04

- Added
  [`forecast.lm()`](https://pkg.robjhyndman.com/forecast/reference/forecast.lm.md)
- Modified
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html) and
  [`print.forecast()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
  to allow non time series forecasts.
- Fixed visibility of
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md).

## forecast 2.13

CRAN release: 2011-02-16

- Fixed bug in
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  when only 1 forecast is specified.
- Added
  [`forecast.stl()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  and
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  functions
- Modified
  [`forecast.ts()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
  to use
  [`stlf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.stl.md)
  if frequency \> 12.
- Made
  [`BoxCox()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.md)
  and
  [`InvBoxCox()`](https://pkg.robjhyndman.com/forecast/reference/BoxCox.md)
  robust to negative values
- Fixed bug in
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  when future=TRUE. There was a bias in the sample paths.

## forecast 2.12

CRAN release: 2011-01-19

- Added
  [`naive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  and
  [`snaive()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md)
  functions.
- Improved handling of seasonal data with frequency \< 1.
- Added lambda argument to
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html).

## forecast 2.11

CRAN release: 2010-11-04

- If MLE in
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  fails (usually because the series is non-stationary), the LS estimate
  is now returned.

## forecast 2.10

- Fixed bug in
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  where the MA parameters were of the wrong sign if estim=“mle” chosen.
- [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  now allowed to have a sequence of missing values at the start of the
  series and end of the series

## forecast 2.09

CRAN release: 2010-10-15

- Fixed bug in
  [`forecast.fracdiff()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  which caused an error when h=1.
- Added shadebars to
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md).
- Fixed bug in
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  to allow plotting when h=1.

## forecast 2.08

CRAN release: 2010-09-22

- Added pp test option for
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  and
  [`ndiffs()`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.md).
- Fixed bug in
  [`simulate.ets()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  which was causing problems when forecasting from some ETS models
  including ETS(M,M,N).

## forecast 2.07

CRAN release: 2010-09-09

- Fixed bug in
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md).
  Previous sample paths when d=2 and future=TRUE were incorrect.
- Changed way color is implemented in
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  to avoid colour changes when the graphics window is refreshed.

## forecast 2.06

CRAN release: 2010-07-29

- Added MLE option for
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md).
- Added
  [`simulate.Arima()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md),
  [`simulate.ar()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  and
  [`simulate.fracdiff()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)

## forecast 2.05

CRAN release: 2010-05-11

- Added
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  and a forecast method to handle ARFIMA models from
  [`arfima()`](https://pkg.robjhyndman.com/forecast/reference/arfima.md)
  and [`fracdiff()`](https://rdrr.io/pkg/fracdiff/man/fracdiff.html).
- Added residuals and fitted methods for fracdiff objects.

## forecast 2.04

CRAN release: 2010-04-16

- Fixed bug in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  that occurred rarely.

## forecast 2.03

CRAN release: 2009-12-24

- Added an option to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  to allow drift terms to be excluded from the models considered.

## forecast 2.02

CRAN release: 2009-12-23

- Fixed bug in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  that occurred when there was an xreg but no drift, approximation=TRUE
  and stepwise=FALSE.

## forecast 2.01

CRAN release: 2009-09-18

- Fixed bug in time index of
  [`croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md)
  output.
- Added further explanation about models to
  [`croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md)
  help file.

## forecast 2.00

CRAN release: 2009-09-07

- Package removed from forecasting bundle

## forecast 1.26 (29 August 2009)

- Added
  [`as.data.frame.forecast()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md).
  This allows
  [`write.table()`](https://rdrr.io/r/utils/write.table.html) to work
  for forecast objects.

## forecast 1.25 (22 July 2009)

- Added argument to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  and
  [`ndiffs()`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.md)
  to allow the ADF test to be used instead of the KPSS test in selecting
  the number of differences.
- Added argument to
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md)
  to allow different colors and line types when plotting prediction
  intervals.
- Modified
  [`forecast.ts()`](https://pkg.robjhyndman.com/forecast/reference/forecast.ts.md)
  to give sensible results with a time series containing fewer than four
  observations.

## forecast 1.24 (9 April 2009)

- Fixed bug in
  [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  to avoid errors when there are missing values in the residuals.
- More informative error messages when
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  fails to find a suitable model.

## forecast 1.23 (22 February 2009)

- Fixed bugs that meant xreg terms in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  sometimes caused errors when stepwise=FALSE.

## forecast 1.22 (30 January 2009)

- Fixed bug that meant regressor variables could not be used with
  seasonal time series in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).

## forecast 1.21 (16 December 2008)

- Fixed bugs introduced in v1.20.

## forecast 1.20 (14 December 2008)

- Updated
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  to allow regression variables.
- Fixed a bug in `print.Arima()` which caused problems when the data
  were inside a data.frame.
- In
  [`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md),
  argument h is now set to the length of the xreg argument if it is not
  null.

## forecast 1.19 (7 November 2008)

- Updated
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  to allow regression variables when refitting an existing model to new
  data.

## forecast 1.18 (6 November 2008)

- Bug fix in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md):
  models with frequency less than 1 would cause R to hang.
- Bug fix in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md):
  models with frequency greater than 12 would not fit due to parameters
  being out of range.
- Default lower and upper bounds on parameters alpha, beta and gamma in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)
  changed to 0.0001 and 0.9999 (instead of 0.01 and 0.99).

## forecast 1.17 (10 October 2008)

- Calculation of BIC did not account for reduction in length of series
  due to differencing. Now fixed in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  and in `print.Arima()`.
- [`tsdiag()`](https://rdrr.io/r/stats/tsdiag.html) now works with ets
  objects.

## forecast 1.16 (29 September 2008)

- Another bug fix in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
  Occasionally the root checking would cause an error. The condition is
  now trapped.

## forecast 1.15 (16 September 2008)

- Bug fix in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
  The series wasn’t always being stored as part of the return object
  when stepwise=FALSE.

## forecast 1.14 (1 August 2008)

- The time series stored in M3 in the Mcomp package did not contain all
  the components listed in the help file. This problem has now been
  fixed.

## forecast 1.13 (16 June 2008)

- Bug in
  [`plot.ets()`](https://pkg.robjhyndman.com/forecast/reference/plot.ets.md)
  fixed so that plots of non-seasonal models for seasonal data now work.
- Warning added to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) if
  the time series contains very large numbers (which can cause numerical
  problems). Anything up to 1,000,000 should be ok, but any larger and
  it is best to scale the series first.
- Fixed problem in
  [`forecast.HoltWinters()`](https://pkg.robjhyndman.com/forecast/reference/forecast.HoltWinters.md)
  where the lower and upper limits were interchanged.

## forecast 1.12 (22 April 2008)

- Objects are now coerced to class ts in
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md). This
  allows it to work with zoo objects.
- A new function
  [`dm.test()`](https://pkg.robjhyndman.com/forecast/reference/dm.test.md)
  has been added. This implements the Diebold-Mariano test for
  predictive accuracy.
- Yet more bug-fixes for
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).

## forecast 1.11 (8 February 2008)

- Modifications to
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  in the case where ML estimation does not work for the chosen model.
  Previously this would return no model. Now it returns the model
  estimated using CSS.
- AIC values reported in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  when trace=TRUE and approximation=TRUE are now comparable to the final
  AIC values.
- Addition of the expsmooth package.

## forecast 1.10 (21 January 2008)

- Fixed bug in
  [`seasadj()`](https://pkg.robjhyndman.com/forecast/reference/seasadj.md)
  so it allows multiple seasonality
- Fixed another bug in `print.Arima()`
- Bug fixes in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
  It was sometimes returning a non-optimal model, and occasionally no
  model at all. Also, additional stationarity and invertibility testing
  is now done.

## forecast 1.09 (11 December 2007)

- A new argument ‘restrict’ has been added to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) with
  default TRUE. If set to FALSE, then the unstable ETS models are also
  allowed.
- A bug in the `print.Arima()` function was fixed.

## forecast 1.08 (21 November 2007)

- AICc and BIC corrected. Previously I had not taken account of the
  sigma^2 parameter when computing the number of parameters.
- [`arima()`](https://rdrr.io/r/stats/arima.html) function changed to
  [`Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.md)
  to avoid the clash with the
  [`arima()`](https://rdrr.io/r/stats/arima.html) function in the stats
  package.
- [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  now uses an approximation to the likelihood when selecting a model if
  the series is more than 100 observations or the seasonal period is
  greater than 12. This behaviour can be over-ridden via the
  approximation argument.
- A new function
  [`plot.ets()`](https://pkg.robjhyndman.com/forecast/reference/plot.ets.md)
  provides a decomposition plot of an ETS model.
- [`predict()`](https://rdrr.io/r/stats/predict.html) is now an alias
  for [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  wherever there is not an existing
  [`predict()`](https://rdrr.io/r/stats/predict.html) method.
- The argument conf has been changed to level in all forecasting methods
  to be consistent with other R functions.
- The functions `gof()` and `forecasterrors()` have been replaced by
  [`accuracy()`](https://generics.r-lib.org/reference/accuracy.html)
  which handles in-sample and out-of-sample forecast accuracy.
- The initialization method used for a non-seasonal ETS model applied to
  seasonal data was changed slightly.
- The following methods for ets objects were added: summary, coef and
  logLik.
- The following methods for Arima objects were added: summary.

## forecast 1.07 (25 July 2007)

- Bug fix in summary of in-sample errors. For ets models with
  multiplicative errors, the reported in-sample values of MSE, MAPE,
  MASE, etc., in [`summary()`](https://rdrr.io/r/base/summary.html) and
  `gof()` were incorrect.
- ARIMA models with frequency greater than 49 now allowed. But there is
  no unit-root testing if the frequency is 50 or more, so be careful!
- Improvements in documentation.

## forecast 1.06 (15 June 2007)

- Bug fix in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
  It would not always respect the stated values of max.p, max.q, max.P
  and max.Q.
- The tseries package is now installed automatically along with the
  forecasting bundle, whereas previously it was only suggested.

## forecast 1.05 (28 May 2007)

- Introduced
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md)
  to provide a stepwise approach to ARIMA modelling. This is much faster
  than the old `best.arima()`.
- The old grid-search method used by `best.arima()` is still available
  by using stepwise=FALSE when calling
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Automated choice of seasonal differences introduced in
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.md).
- Some small changes to the starting values of ets models.
- Fixed a bug in applying
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) to
  new data using a previously fitted model.

## forecast 1.04 (30 January 2007)

- Added include.drift to [`arima()`](https://rdrr.io/r/stats/arima.html)
- Fixed bug in seasonal forecasting with
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md)

## forecast 1.03 (20 October 2006)

- Fixed some DOS line feed problems that were bothering unix users.

## forecast 1.02 (12 October 2006)

- Added AICc option to
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) and
  `best.arima()`.
- Corrected bug in calculation of fitted values in ets models with
  multiplicative errors.

## forecast 1.01 (25 September 2006)

- Modified
  [`ndiffs()`](https://pkg.robjhyndman.com/forecast/reference/ndiffs.md)
  so that the maximum number of differences allowed is 2.

## forecast 1.0 (31 August 2006)

- Added MASE to `gof()`.
- [`croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md)
  now returns fitted values and residuals.
- [`arima()`](https://rdrr.io/r/stats/arima.html) no longer allows
  linear trend + ARMA errors by default. Also, drift in non-stationary
  models can be turned off.
- This version is the first to be uploaded to CRAN.

## forecast 0.99992 (8 August 2006)

- Corrections to help files. No changes to functionality.

## forecast 0.99991 (2 August 2006)

- More bug fixes.
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) now
  converges to a good model more often.

## forecast 0.9999 (1 August 2006)

- Mostly bug fixes.
- A few data sets have been moved from fma to forecast as they are not
  used in my book.
- [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) is
  now considerably slower but gives better results. Full optimization is
  now the only option (which is what slows it down). I had too many
  problems with poor models when partial optimization was used. I’ll
  work on speeding it up sometime, but this is not a high priority. It
  is fast enough for most use. If you really need to forecast 1000
  series, run it overnight.
- In [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md),
  I’ve experimented with new starting conditions for optimization and it
  seems to be fairly robust now.
- Multiplicative error models can no longer be applied to series
  containing zeros or negative values. However, the forecasts from these
  models are not constrained to be positive.

## forecast 0.999 (27 July 2006)

- The package has been turned into three packages forming a bundle. The
  functions and a few datasets are still in the forecast package. The
  data from Makridakis, Wheelwright and Hyndman (1998) is now in the fma
  package. The M-competition data is now in the Mcomp package. Both fma
  and Mcomp automatically load forecast.
- This is the first version available on all operating systems (not just
  Windows).
- `pegels()` has been replaced by
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) only
  fits the model; it doesn’t produce forecasts. To get forecasts, apply
  the forecast function to the ets object.
- [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md) has
  been completely rewritten which makes it slower, but much easier to
  maintain. Different boundary conditions are used and a different
  optimizer is used, so don’t expect the results to be identical to what
  was done by the old `pegels()` function. To get something like the
  results from the old `pegels()` function, use
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html) on
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).
- [`simulate.ets()`](https://pkg.robjhyndman.com/forecast/reference/simulate.ets.md)
  added to simulate from an ets model.
- Changed name of cars to auto to avoid clash with the cars data in the
  datasets package.
- arima2 functionality is now handled by
  [`arima()`](https://rdrr.io/r/stats/arima.html) and pegels2
  functionality is now handled by
  [`ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.md).
- `best.arima()` now allows the option of BIC to be used for model
  selection.
- Croston’s method added in function
  [`croston()`](https://pkg.robjhyndman.com/forecast/reference/forecast.croston_model.md).
- `ts.display()` renamed as
  [`tsdisplay()`](https://pkg.robjhyndman.com/forecast/reference/tsdisplay.md)
- `mean.f()` changed to
  [`meanf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.mean_model.md),
  `theta.f()` changed to
  [`thetaf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.theta_model.md),
  `rw.f()` changed to
  [`rwf()`](https://pkg.robjhyndman.com/forecast/reference/forecast.rw_model.md),
  `seasonaldummy.f()` to
  [`seasonaldummyf()`](https://pkg.robjhyndman.com/forecast/reference/seasonaldummy.md),
  `sindex.f()` to
  [`sindexf()`](https://pkg.robjhyndman.com/forecast/reference/sindexf.md),
  and `spline.f()` to
  [`splinef()`](https://pkg.robjhyndman.com/forecast/reference/forecast.spline_model.md).
  These changes are to avoid potential problems if anyone introduces an
  ‘f’ class.

## forecast 0.994 (4 October 2004)

- Fixed bug in [`arima()`](https://rdrr.io/r/stats/arima.html) which
  caused [`predict()`](https://rdrr.io/r/stats/predict.html) to
  sometimes fail when there was no xreg term.
- More bug fixes in handling regression terms in arima models.
- New `print.Arima()` function for more informative output.

## forecast 0.993 (20 July 2004)

- Added forecast function for structural time series models obtained
  using [`StructTS()`](https://rdrr.io/r/stats/StructTS.html).
- Changed default parameter space for `pegels()` to force admissibility.
- Added option to `pegels()` to allow restriction to models with finite
  forecast variance. This restriction is imposed by default.
- Fixed bug in
  [`arima.errors()`](https://pkg.robjhyndman.com/forecast/reference/arima.errors.md).
  Changes made to [`arima()`](https://rdrr.io/r/stats/arima.html) meant
  [`arima.errors()`](https://pkg.robjhyndman.com/forecast/reference/arima.errors.md)
  was often returning an error message.
- Added a namespace to the package making fewer functions visible to the
  user.

## forecast 0.99 (21 May 2004)

- Added automatic selection of order of differencing for `best.arima()`.
- Added possibility of linear trend in arima models.
- In `pegels()`, option added to allow parameters of an exponential
  smoothing model to be in the ‘admissible’ (or invertible) region
  rather than within the usual (0,1) region.
- Fixed some bugs in `pegels()`.
- Included all M1 and M3 data and some functions to subset and plot
  them.
- Note: This package will only work in R1.9 or later.

## forecast 0.98 (23 August 2003)

- Added facilities in `pegels()`. o It is now possible to specify
  particular values of the smoothing parameters rather than always use
  the optimized values. If none are specified, the optimal values are
  still estimated as before. o It is also possible to specify upper and
  lower bounds for each parameter separately.
- New function: `theta.f()`. This implements the Theta method which did
  very well in the M3 competition.
- A few minor problems with `pegels()` fixed and a bug in
  `forecast.plot()` that meant it didn’t work when the series contained
  missing values.

## forecast 0.972 (11 July 2003)

- Small bug fix: `pegels()` did not return correct model when model was
  partially specified.

## forecast 0.971 (10 July 2003)

- Minor fixes to make sure the package will work with R v1.6.x. No
  changes to functionality.

## forecast 0.97 (9 July 2003)

- Fully automatic forecasting based on the state space approach to
  exponential smoothing has now been added. For technical details, see
  Hyndman, Koehler, Snyder and Grose (2002).
- Local linear forecasting using cubic smoothing splines added. For
  technical details, see Hyndman, King, Pitrun and Billah (2002).

## forecast 0.96 (15 May 2003)

- Many functions rewritten to make use of methods and classes.
  Consequently several functions have had their names changed and many
  arguments have been altered. Please see the help files for details.
- Added functions
  [`forecast.Arima()`](https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.md)
  and `forecat.ar()`
- Added functions `gof()` and
  [`seasadj()`](https://pkg.robjhyndman.com/forecast/reference/seasadj.md)
- Fixed bug in
  [`plot.forecast()`](https://pkg.robjhyndman.com/forecast/reference/plot.forecast.md).
  The starting date for the plot was sometimes incorrect.
- Added residuals components to `rw.f()` and `mean.f()`.
- Made several changes to ensure compatibility with Rv1.7.0.
- Removed a work-around to fix a bug in
  [`monthplot()`](https://rdrr.io/r/stats/monthplot.html) command
  present in R v\<=1.6.2.
- Fixed the motel data set (columns were swapped)
