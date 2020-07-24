
## Submission info

Resubmission with your suggested changes from last submission (2020-07-23):

* Your remark on the use of `options()` without a following `on.exit()`: This is only the case in `zzz.R`. Here, only package specific *prefixed* options are added. No other user options are changed and the prefixes avoid clashes with other option settings. IMHO `on.exit()` does not apply here. (BTW: The code is a exact copy of the the devtool's package zzz.R code and recommended as good practice here http://r-pkgs.had.co.nz/r.html).

* All `T` and `F` values have been replaced by `TRUE` and `FALSE`.

* copyright holder [cph] role added to DESCRIPTION.

* All instances of `system.file` paths have been changed as recommended

* `\dontrun{}` was removed in example

**Thanks for your helpful remarks!**



## Test environments

* windows 10 (local desktop), R 4.0.2
* ubuntu 16.04.6 LTS (travis-ci), R 4.0.0
* windows server x64 (appveyor.com) R 4.0.2
* win-builder (devel and release)
* R-hub builder

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.

