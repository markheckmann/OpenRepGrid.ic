
## Submission info

Resubmission with suggested changes from first submission (2020-03-01):

* DOI of accompanying publication added to DESCRIPTION 
* `seed` is now an argument for `set.seed` inside the function (is needed)
* `on.exit` added to restore user par when function is exited
* `\dontrun{}` replaced by `if(interactive())` in examples of `ic()` function

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

