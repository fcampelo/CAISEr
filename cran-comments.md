## Changes and notes
* This is a maintenance update.
* Added function to consolidate partial results saved to file (`consolidate.partial.results()`)
* `run_nreps2()` now adds instance alias as a field to the output structure
* fixed rare bug in `calc_se()` that resulted in `NaN` if two vectors with the 
same sample mean and same sample variance were passed as arguments.
* Fixed minor problem with print function in the Vignette.

## Test environments
* local OS X 10.12.6, R 3.5.0
* Ubuntu 14.04.5 LTS (on travis-ci v 3.8.0), R version 3.5.0
* win-builder (release and devel)

## R CMD check results
