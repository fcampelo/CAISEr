## Changes and notes
* Major change in the package. Not backward compatible.
* Added function `calc_nreps()` to calculate the number of repetitions in the case of multiple (>2) algorithms. This replaces `calc_nreps2()`
* function `calc_se()` now performs point estimate and standard error 
calculations for all pairs of interest in the case of multiple algorithms

## Test environments
* macOS Mojave 10.14.5, R 3.6.0 (2019-04-26), x86_64-apple-darwin15.6.0 (64-bit)
* Ubuntu 14.04.5 LTS (on travis-ci v 3.10.1), R version 3.5.0
* win-builder (release and devel)

## R CMD check results  
