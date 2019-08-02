# CAISEr 1.0.14
* CAISEr plotting now returns a list of ggplot objects and relevant dataframes, and plots a nested plot of relevant information.
* fixed bug when running experiment with two algorithms (due to 
the default `drop = TRUE` when subsetting rows in `se_param()`/`se_boot()`).

# CAISEr 1.0.6
* Added plot function to class `CAISEr`

# CAISEr 1.0.5
* Major update: package can now deal with comparisons of multiple (>2) algorithms. 
This version is **not** backward compatible with versions 0.xx. To use the previous versions (e.g., to replicate the results from the paper "Sample Size Estimation 
for Power and Accuracy in the Experimental Comparison of Algorithms", Journal of 
Heuristics 2019), install the previous version from 
[https://cran.r-project.org/src/contrib/CAISEr_0.3.3.tar.gz](https://cran.r-project.org/src/contrib/CAISEr_0.3.3.tar.gz).
* Replaced function `calc_nreps2()` by `calc_nreps()` to calculate the number of
repetitions in the case of multiple (>2) algorithms. 
* function `calc_se()` now performs point estimate and standard error 
calculations for all pairs of interest in the case of multiple algorithms.
* function `calc_instances()` now returns the number of required instances to
maintain the desired power for the family of experiments, based on Holm's 
step-down correction. It also returns the sequence of alpha values and the 
corresponding power to detect the effect size `d`, for each comparison using 
Holm's method.
* Removed class `CAISER_powercurve` and all associated methods (print, summary, 
plot) as these were (very) rarely used.
* Added class `nreps` with new summary/plotting functions.
* Updated `print` and `summary` methods.
* The package now supports saving and (re)loading partially executed experiments.
 
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
