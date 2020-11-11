## Test environments
* local R installation (Fedora-31): R 3.6.3
* local R installation (MacOS): R 4.0.3
* Github actions (ubuntu 18.04): oldrel, release
* Github actions (Windows): oldrel, release
* Github actions (MacOS): oldrel, release
* R-hub (fedora-clang-devel): r-devel
* R-hub (windows-x86_64-devel): r-devel
* R-hub (ubuntu-gcc-release): r-release
* win-builder: devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Re-submission fixes
* Remove all attempts to modify the global environment in functions from
`DefineAndUpdateVariables.R` and `xsdUtils.R` files.
* Shorten the package title.