## Changes and notes
* Fixed minor bug in print function
* Added new example (plus associated functions) to `calc_nreps2()`
* Improved summary and plot functions for CAISErPowercurve objects
* Added options to `calc_power_curve()` to determine the range of effect sizes to consider.
* Added option in `run_experiment()` to force the use of all available instances.
* `run_experiment()` can now be run in parallel using multiple cores.
* Added packages _parallel_ and _pbmcapply_ to **Imports**.
* Added option to `run_experiment()` and `calc_nreps2()` to save results to file

## Test environments
* local OS X 10.12.6, R 3.5.0
* Ubuntu 14.04.5 LTS (on travis-ci v 3.8.0), R version 3.5.0
* win-builder (release and devel)

## R CMD check results
R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded
