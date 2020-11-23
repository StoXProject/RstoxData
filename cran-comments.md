## Test environments
* local R installation (Fedora-31): R 3.6.3
* local R installation (MacOS): R 4.0.3
* Github actions (ubuntu 16.04): oldrel, release
* Github actions (Windows): oldrel, release
* Github actions (MacOS): oldrel, release
* R-hub (fedora-clang-devel): r-devel
* R-hub (windows-x86_64-devel): r-devel
* R-hub (ubuntu-gcc-release): r-release
* win-builder: devel

## R CMD check results

0 errors | 0 warnings | 1 note

❯ checking CRAN incoming feasibility ... NOTE

  Maintainer: 'Ibrahim Umar <ibrahim.umar@hi.no>'

  Days since last update: 5

## New version submission

* Remove `file.copy()` to location outside of `tempdir()` in tests.
* New version with several bug fixes and improvements.