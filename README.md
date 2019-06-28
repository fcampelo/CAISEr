## CAISEr package
[![Build Status](https://api.travis-ci.org/fcampelo/CAISEr.png)](https://travis-ci.org/fcampelo/CAISEr) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/CAISEr)](https://CRAN.R-project.org/package=CAISEr)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/CAISEr)](https://CRAN.R-project.org/package=CAISEr)

***

## CAISEr: Comparing Algorithms with Iterative Sample-size Estimation in R
Felipe Campelo ([f.campelo@aston.ac.uk](mailto:f.campelo@aston.ac.uk)) [1,2]  
Fernanda Takahashi ([fernandact@ufmg.br](mailto:fernandact@ufmg.br))[2]

[1] Department of Computer Science, Aston University, Birmingham UK  
[2] Operations Research and Complex Systems Laboratory, Universidade Federal de Minas Gerais, Belo Horizonte, Brazil  

***

Implementation of R package _CAISEr_, with routines for automatically 
determining the sample size needed for performing comparative experiments with 
algorithms.

To install the most up-to-date version directly from Github, simply type:

```
library(devtools)
devtools::install_github("fcampelo/CAISEr@devel")
```

The most recent CRAN release of the package is also available for installation directly 
from the R prompt, using:

```
install.packages("CAISEr")
```

For instructions and examples of use, please take a look at the vignette 
_Adapting Algorithms for CAISEr_, and at the package documentation, particularly 
functions `run_experiment()` and `run_nreps()`.

Please send any bug reports, questions, suggestions, beers (we can always hope!) directly to the package authors listed at the top of this document.

Cheers,  
Felipe
