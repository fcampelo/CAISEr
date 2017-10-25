## CAISEr package
[![Build Status](https://api.travis-ci.org/fcampelo/CAISEr.png)](https://travis-ci.org/fcampelo/CAISEr) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/CAISEr)](https://CRAN.R-project.org/package=CAISEr)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/CAISEr)](https://CRAN.R-project.org/package=CAISEr)

***

## CAISEr: Comparing Algorithms with Iterative Sample-size Estimation in R
Felipe Campelo ([fcampelo@ufmg.br](mailto:fcampelo@ufmg.br)) and Fernanda Takahashi ([fernandact@ufmg.br](mailto:fernandact@ufmg.br))  
Operations Research and Complex Systems Laboratory - ORCS Lab  
Universidade Federal de Minas Gerais  
Belo Horizonte, Brazil

***

Implementation of R package _CAISEr_, with routines for automatically 
determining the sample size needed for performing comparative experiments with 
algorithms.

To install the most up-to-date version directly from Github, simply type:

```
library(devtools)
devtools::install_github("fcampelo/CAISEr")
```

The package will also be available (hopefully soon) for installation directly 
from the CRAN repository, using:

```
install.packages("CAISEr")
```

For instructions and examples of use, please take a look at the vignette 
_Adapting Algorithms for CAISEr_, and at the package documentation, particularly 
that of functions `run_experiment()` and `run_nreps2()`.


Please send any bug reports, questions, suggestions, chocolate (to 
Fernanda) or beers (to Felipe - we can always hope!) directly to the package 
authors listed at the top of this document.

Cheers,  
Felipe
