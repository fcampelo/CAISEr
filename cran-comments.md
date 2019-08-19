## Changes and notes
* Minor update: 
  * removed `grid` and `lattice` from **Imports** field (not needed anymore) to remove CRAN NOTEs.
  * fixed minor URL problem and added usage example of `plot` method in the vignette.
  * Updated DESCRIPTION with links to published work describing the methods implemented in the package.

## Test environments
## Test environments
* macOS Mojave 10.14.6, R version 3.6.1, x86_64-apple-darwin15.6.0 (64-bit), 
using `devtools::check()`
* Ubuntu 14.04.5 LTS (on **travis-ci v6.2.1**), R version 3.6.1
* win-builder, using `devtools::check_win_devel()` and  `devtools::check_win_release()`
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit, using `devtools::check_rhub()`
* Ubuntu Linux 16.04 LTS, R-release, GCC, using `devtools::check_rhub()`
* Fedora Linux, R-devel, clang, gfortran, using `devtools::check_rhub()`

## R CMD check results  
* 0 errors | 0 warnings | 0 notes ; R CMD check succeeded
