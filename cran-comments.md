## Test environments

* KDE neon 6.5.4 based on ubuntu 24.04 (local): R 4.5.2
* macOS (on GitHub Actions): release
* windows (on GitHub Actions): release
* ubuntu 24.04.3 (on GitHub Actions): devel, release, oldrel
* win-builder: devel, release, oldrelease

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdep checks

All reverse dependencies have been checked, and this update causes new errors in
three packages. I provided patches to fix the errors via GitHub pull requests 
on 16 December, asked the maintainers to submit new versions to CRAN.

* https://github.com/xqnwang/conformalForecast/pull/4
* https://github.com/frehbach/EventDetectR/pull/24
* https://github.com/ellisp/forecastHybrid/pull/101

