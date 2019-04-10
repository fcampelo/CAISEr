# CAISEr 0.4.0
* Major update: package can now deal with comparisons of multiple (>2) algorithms.
* added function `calc_nreps()` to calculate the number of repetitions in the case of multiple (>2) algorithms.
* added function `calc_sek()` to perform the standard error calculations for the case with multiple algorithms

# CAISEr 0.3.3
* fixed problem with printing version in the vignette.

# CAISEr 0.3.2
* fixed rare bug in `calc_se()` that resulted in `NaN` if two vectors with the 
same sample mean and same sample variance were passed as arguments.

# CAISEr 0.3.1
* Added function to consolidate partial results saved to file (`consolidate.partial.results()`)
* Minor improvements to saving partial results to file: users can now select arbitrary directory for saving

# CAISEr 0.3
* `run_experiment()` can now be run in parallel using multiple cores.
* `run_experiment()` and `calc_nreps2()` can now save results to files.

# CAISEr 0.2.4
* `run_experiment()` now forces the use of all available instances if `power >= 1`.

# CAISEr 0.2.3
* Improved plot and summary functions for `CAISErPowercurve` objects.
* Added options to `calc_power_curve()` to determine the range of effect sizes to consider.

# CAISEr 0.2.2
* Added new example and use case to `calc_nreps2()`

# CAISEr 0.2.1
* Minor fixes, particularly in printing function.

# CAISEr 0.2.0
* Initial release on CRAN.
