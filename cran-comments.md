Attempted to fix issue regarding defining R_NO_REMAP before R headers from C++ 
as advised by email from CRAN

Note that it is not possible for us to test this, but I think we have done 
what is required.

## Test environments

* KDE neon 6.0 based on ubuntu 22.04 (local): R 4.3.2
* macOS (on GitHub Actions): release
* windows (on GitHub Actions): release
* ubuntu 22.04.4 (on GitHub Actions): devel, release, oldrel
* win-builder: devel, release, oldrelease

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdep checks

All reverse dependencies have been checked with no new errors detected.
