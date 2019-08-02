## Changes and notes
* Change ownership e-mail from `fcampelo@ufmg.br` to `f.campelo@aston.ac.uk` due to change in professional affiliation.
* Minor update: added plotting function to class `CAISEr`
* Minor update: fixed bug when running experiment with two algorithms (due to 
the default `drop = TRUE` when subsetting rows in `se_param()`/`se_boot()`).

## Test environments
* macOS Mojave 10.14.5, R 3.6.0 (2019-04-26), x86_64-apple-darwin15.6.0 (64-bit), 
using `devtools::check()`
* win-builder, using `devtools::check_win_devel()`, `devtools::check_win_release()`
* Ubuntu 14.04.5 LTS (on **travis-ci v6.2.0**), R version 3.6.0

## R CMD check results  
* 0 errors | 0 warnings | 0 notes
