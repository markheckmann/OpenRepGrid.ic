## Submission info

Implement changes suggested by JOSS <https://joss.theoj.org/> reviewers in peer-review process

## Test environments

* Windows 10 (local desktop), R 4.2.1
* Ubuntu 16.04.6 LTS (travis-ci), R 4.1.0
* Windows Server x64 (appveyor.com) R 4.1.0
* win-builder (devel and release)
* R-hub builder

## R CMD check results

0 errors | 0 warnings | 1 notes

  Imports includes 23 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable. Move as many as possible to Suggests and
  use conditionally.

   => THIS IS OKAY FOR US

## Reverse dependencies

There are no reverse dependencies.
