## Resubmission
This is a resubmission. In this version I have:

* Resolved reverse dependency issues flagged in the previous submission.

## Test environments
* ubuntu 16.04 (on travis-ci), R 3.5.1
* ubuntu 18.04
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependency checks

Reverse dependency checks were performed where possible

## Existing CRAN check ERRORs

A recent error caused by a regression of `stats::reformulate()` is resolved in this submission.
In R <= 3.5.3 it is possible to use reformulate with syntactically invalid names (which would treated appropriately as a symbol/name, and not parsed).