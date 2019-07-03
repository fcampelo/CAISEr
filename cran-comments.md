## Changes and notes
* Change ownership e-mail from `fcampelo@ufmg.br` to `f.campelo@aston.ac.uk`
* Major change in the package. **Not backward compatible with versions 0.XX**
  * The previous version can still be used, and is available both from
[https://cran.r-project.org/src/contrib/CAISEr_0.3.3.tar.gz](https://cran.r-project.org/src/contrib/CAISEr_0.3.3.tar.gz) or using `devtools::install_github('fcampelo/CAISEr@v0.3.3')`
* Function `calc_nreps()` replaces `calc_nreps2()`, now calculates the number of 
repetitions in the case of multiple (>2) algorithms. 
* function `calc_se()` now performs point estimate and standard error 
calculations for all pairs of interest in the case of multiple algorithms.


## Test environments
* macOS Mojave 10.14.5, R 3.6.0 (2019-04-26), x86_64-apple-darwin15.6.0 (64-bit), 
using `devtools::check()`
* win-builder, using `devtools::check_win_devel()` and `devtools::check_win_release()`
* Ubuntu 14.04.5 LTS (on travis-ci v 3.10.1), R version 3.5.0

## R CMD check results  
* 0 errors | 0 warnings | 0 notes
