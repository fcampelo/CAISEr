## Changes and notes
* This is a maintenance update with minor improvements / fixes.
* Added function to consolidate partial results saved to file (`consolidate.partial.results()`)
* `run_nreps2()` now adds instance alias as a field to the output structure
* fixed rare bug in `calc_se()` that resulted in `NaN` if two vectors with the 
same sample mean and same sample variance were passed as arguments.
* Fixed minor problem with print function in the Vignette.

## Test environments
* local macOS High Sierra 10.13.5, R version 3.5.0
* Ubuntu 14.04.5 LTS (on travis-ci v 3.10.1), R version 3.5.0
* win-builder (release and devel)

## R CMD check results  
0 errors | 0 warnings | 0 notes  
R CMD check succeeded
