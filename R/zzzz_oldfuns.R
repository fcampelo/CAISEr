#' Determine sample sizes for a set of algorithms on a single problem instance
#'
#' Iteratively calculates the required sample sizes for K algorithms
#' on a given problem instance, so that the standard errors of the estimates of
#' the pairwise differences in performance is controlled at a predefined level.
#'
#' @section Instance:
#' Parameter `instance` must be a named list containing all relevant parameters
#' that define the problem instance. This list must contain at least the field
#' `instance$FUN`, with the name of the function implementing the problem
#' instance, that is, a routine that calculates y = f(x). If the instance
#' requires additional parameters, these must also be provided as named fields.
#'
#' @section Algorithms:
#' Object `algorithms` is a list in which each component is a named
#' list containing all relevant parameters that define an algorithm to be
#' applied for solving the problem instance. In what follows `algorithm[[k]]`
#' refers to any algorithm specified in the `algorithms` list.
#'
#' `algorithm[[k]]` must contain an `algorithm[[k]]$FUN` field, which is a
#' character object with the name of the function that calls the algorithm; as
#' well as any other elements/parameters that `algorithm[[k]]$FUN` requires
#' (e.g., stop criteria, operator names and parameters, etc.).
#'
#' The function defined by the routine `algorithm[[k]]$FUN` must have the
#' following structure: supposing that the list in `algorithm[[k]]` has
#' fields `algorithm[[k]]$FUN = "myalgo"`, `algorithm[[k]]$par1 = "a"` and
#' `algorithm$par2 = 5`, then:
#'
#'    \preformatted{
#'          myalgo <- function(par1, par2, instance, ...){
#'                # do stuff
#'                # ...
#'                return(results)
#'          }
#'    }
#'
#' That is, it must be able to run if called as:
#'
#'    \preformatted{
#'          # remove '$FUN' and '$alias' field from list of arguments
#'          # and include the problem definition as field 'instance'
#'          myargs          <- algorithm[names(algorithm) != "FUN"]
#'          myargs          <- myargs[names(myargs) != "alias"]
#'          myargs$instance <- instance
#'
#'          # call function
#'          do.call(algorithm$FUN,
#'                  args = myargs)
#'    }
#'
#' The `algorithm$FUN` routine must return a list containing (at
#' least) the performance value of the final solution obtained, in a field named
#' `value` (e.g., `result$value`) after a given run.
#'
#' @section Initial Number of Observations:
#' In the **general case** the initial number of observations per algorithm
#' (`nstart`) should be relatively high. For the parametric case
#' we recommend between 10 and 20 if outliers are not expected, or between 30
#' and 50 if that assumption cannot be made. For the bootstrap approach we
#' recommend using at least 20. However, if some distributional assumptions can
#' be made - particularly low skewness of the population of algorithm results on
#' the test instances), then `nstart` can in principle be as small as 5 (if the
#' output of the algorithms were known to be normal, it could be 1).
#'
#' In general, higher sample sizes are the price to pay for abandoning
#' distributional assumptions. Use lower values of `nstart` with caution.
#'
#' @section Pairwise Differences:
#' Parameter `dif` informs the type of difference in performance to be used
#' for the estimation (\eqn{\mu_a} and \eqn{\mu_b} represent the mean
#' performance of any two algorithms on the test instance, and \eqn{mu}
#' represents the grand mean of all algorithms given in `algorithms`):
#'
#' - If `dif == "perc"` and `comparisons == "all.vs.first"`, the estimated quantity is
#'    \eqn{\phi_{1b} = (\mu_1 - \mu_b) / \mu_1 = 1 - (\mu_b / \mu_1)}.
#'
#' - If `dif == "perc"` and `comparisons == "all.vs.all"`, the estimated quantity is
#'    \eqn{\phi_{ab} = (\mu_a - \mu_b) / \mu}.
#'
#' - If `dif == "simple"` it estimates \eqn{\mu_a - \mu_b}.
#'
#' @param instance a list object containing the definitions of the problem
#'    instance.
#'    See Section `Instance` for details.
#' @param algorithms a list object containing the definitions of all algorithms.
#'    See Section `Algorithms` for details.
#' @param se.max desired upper limit for the standard error of the estimated
#'        difference between pairs of algorithms. See Section
#'        `Pairwise Differences` for details.
#' @param dif type of difference to be used. Accepts "perc" (for percent
#'          differences) or "simple" (for simple differences)
#' @param comparisons type of comparisons being performed. Accepts "all.vs.first"
#'          (in which cases the first object in `algorithms` is considered to be
#'          the reference algorithm) or "all.vs.all" (if there is no reference
#'          and all pairwise comparisons are desired).
#' @param method method to use for estimating the standard errors. Accepts
#'          "param" (for parametric) or "boot" (for bootstrap)
#' @param nstart initial number of algorithm runs for each algorithm.
#'      See Section `Initial Number of Observations` for details.
#' @param nmax maximum **total** allowed sample size.
#' @param seed seed for the random number generator
#' @param boot.R number of bootstrap resamples to use (if `method == "boot"`)
#' @param ncpus number of cores to use
#' @param force.balanced logical flag to force the use of balanced sampling for
#'        the algorithms on each instance
#' @param save.to.file logical flag: should the results be saved to a file?
#' @param load.from.file logical flag: should the results be loaded from a file?
#' @param folder directory to save/load files
#'
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{instance} - alias for the problem instance considered
#'    \item \code{Xk} - list of observed performance values for all `algorithms`
#'    \item \code{Nk} - vector of sample sizes generated for each algorithm
#'    \item \code{Diffk} - data frame with point estimates, standard errors and
#'    other information for all algorithm pairs of interest
#'    \item \code{seed} - seed used for the PRNG
#'    \item \code{dif} - type of difference used
#'    \item \code{method} - method used ("param" / "boot")
#'    \item \code{comparisons} - type of pairings ("all.vs.all" / "all.vs.first")
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@gmail.com})
#'
#' @references
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms. Journal of Heuristics 25(2):305-338, 2019.
#' - P. Mathews.
#'    Sample size calculations: Practical methods for engineers and scientists.
#'    Mathews Malnar and Bailey, 2010.
#' -  A.C. Davison, D.V. Hinkley:
#'    Bootstrap methods and their application. Cambridge University Press (1997)
#' -  E.C. Fieller:
#'     Some problems in interval estimation. Journal of the Royal Statistical
#'     Society. Series B (Methodological) 16(2), 175–185 (1954)
#' - V. Franz:
#'    Ratios: A short guide to confidence limits and proper use (2007).
#'    https://arxiv.org/pdf/0710.2024v1.pdf
#' - D.C. Montgomery, C.G. Runger:
#'    Applied Statistics and Probability for Engineers, 6th ed. Wiley (2013)
#'
#' @export
#'
#' @examples
#' # Example using dummy algorithms and instances. See ?dummyalgo for details.
#' # We generate 4 dummy algorithms, with true means 15, 10, 30, 15; and true
#' # standard deviations 2, 4, 6, 8.
#' algorithms <- mapply(FUN = function(i, m, s){
#'                           list(FUN   = "dummyalgo",
#'                                alias = paste0("algo", i),
#'                                distribution.fun  = "rnorm",
#'                                distribution.pars = list(mean = m, sd = s))},
#'                      i = c(alg1 = 1, alg2 = 2, alg3 = 3, alg4 = 4),
#'                      m = c(15, 10, 30, 15),
#'                      s = c(2, 4, 6, 8),
#'                      SIMPLIFY = FALSE)
#'
#' # Make a dummy instance with a centered (zero-mean) exponential distribution:
#' instance = list(FUN = "dummyinstance", distr = "rexp", rate = 5, bias = -1/5)
#'
#' se.max = 0.05
#' dif = "perc"
#' comparisons = "all.vs.all"
#' method = "param"
#' seed = 1234
#' nstart = 20
#' nmax   = 1000
#' ncpus  = 1
#'
#' myreps <- calc_nreps(instance = instance, algorithms = algorithms,
#'                      se.max = se.max,   dif = dif,
#'                      comparisons = comparisons, method = method,
#'                      nstart = nstart, nmax = nmax, seed = seed)
#' myreps$Diffk

# TESTED: OK

# calc_nreps_old <- function(instance,            # instance parameters
#                        algorithms,          # algorithm parameters
#                        se.max,              # desired (max) standard error
#                        dif = "simple",      # type of difference
#                        comparisons = "all.vs.all", # differences to consider
#                        method = "param",    # method ("param", "boot")
#                        nstart = 20,         # initial number of samples
#                        nmax   = 200,        # maximum allowed sample size
#                        seed   = NULL,       # seed for PRNG
#                        boot.R = 499,        # number of bootstrap resamples
#                        ncpus  = 1,          # number of cores to use
#                        force.balanced = FALSE,   # force balanced sampling?
#                        save.to.file  = FALSE,    # save results to tmp file?
#                        load.from.file = FALSE,   # load results from file?
#                        folder = "./nreps_files") # directory to save tmp file
# {
#
#   # ========== Error catching ========== #
#   assertthat::assert_that(
#     is.list(instance),
#     assertthat::has_name(instance, "FUN"),
#     is.list(algorithms),
#     all(sapply(X = algorithms, FUN = is.list)),
#     all(sapply(X = algorithms,
#                FUN = function(x){assertthat::has_name(x, "FUN")})),
#     is.numeric(se.max) && length(se.max) == 1,
#     dif %in% c("simple", "perc"),
#     comparisons %in% c("all.vs.all", "all.vs.first"),
#     method %in% c("param", "boot"),
#     assertthat::is.count(nstart),
#     is.infinite(nmax) || assertthat::is.count(nmax),
#     nmax >= length(algorithms) * nstart,
#     is.null(seed) || seed == seed %/% 1,
#     assertthat::is.count(boot.R), boot.R > 1,
#     is.logical(force.balanced), length(force.balanced) == 1,
#     is.logical(save.to.file), length(save.to.file) == 1,
#     is.logical(load.from.file), length(load.from.file) == 1)
#   # ==================================== #
#
#   # set PRNG seed
#   if (is.null(seed)) {
#     if (!exists(".Random.seed")) stats::runif(1)
#     seed <- .Random.seed #i.e., do not change anything
#   } else{
#     set.seed(seed)
#   }
#
#   # Get/set instance alias
#   if (!("alias" %in% names(instance))) {
#     instance$alias <- instance$FUN
#   }
#
#   if (load.from.file){
#     # Get the filename
#     filename <- paste0(folder, "/",
#                        instance$alias,
#                        ".rds")
#
#     if (file.exists(filename)){
#       output <- readRDS(filename)
#       cat("\nSampling of instance", instance$alias, "loaded from file.")
#       return(output)
#     } else
#       cat("\n**NOTE: Instance file", filename, "not found.**")
#   }
#
#   # Echo some information for the user
#   cat("\nSampling algorithms on instance", instance$alias, ": ")
#
#   # generate initial samples
#   Nk <- rep(nstart, length(algorithms))
#   Xk <- parallel::mcmapply(FUN      = get_observations,
#                            algo     = algorithms,
#                            n        = Nk,
#                            MoreArgs = list(instance = instance),
#                            mc.cores = ncpus,
#                            SIMPLIFY = FALSE)
#
#   # Calculate point estimates, SEs, and sample size ratios (current x optimal)
#   Diffk <- calc_se(Xk     = Xk,
#                    dif    = dif,
#                    comparisons = comparisons,
#                    method = method,
#                    boot.R = boot.R)
#
#   while(any(Diffk$SE > se.max) & (sum(Nk) < nmax)){
#     # Echo something for the user
#     if (!(sum(Nk) %% nstart)) cat(".")
#
#     if (force.balanced) {
#       # Generate a single new observation for each algorithm
#       newX <- parallel::mcmapply(FUN      = get_observations,
#                                  algo     = algorithms,
#                                  n        = 1,
#                                  MoreArgs = list(instance = instance),
#                                  mc.cores = ncpus,
#                                  SIMPLIFY = FALSE)
#
#       # Append new observation to each algo list and update sample size counters
#       Xk <- mapply(FUN = c, Xk, newX,
#                    SIMPLIFY = FALSE)
#       Nk <- Nk + 1
#     } else {
#       # Get pair that has the worst SE
#       worst.se <- Diffk[which.max(Diffk$SE), ]
#
#       # Determine algorithm that should receive a new observation
#       if (worst.se$r <= worst.se$ropt){
#         ind <- worst.se[1, 1]
#       } else {
#         ind <- worst.se[1, 2]
#       }
#       # Generate new observation and update Nk counter
#       Xk[[ind]] <- c(Xk[[ind]],
#                      get_observations(algo = algorithms[[ind]],
#                                       instance = instance,
#                                       n = 1))
#       Nk[ind] <- Nk[ind] + 1
#
#       # Recalculate point estimates, SEs, and sample size ratios
#       Diffk <- calc_se(Xk     = Xk,
#                        dif    = dif,
#                        comparisons   = comparisons,
#                        method = method,
#                        boot.R = boot.R)
#     }
#   }
#
#   # Assemble output list
#   names(Nk) <- lapply(algorithms, function(x)x$alias)
#   output    <- list(instance    = instance$alias,
#                     Xk          = Xk,
#                     Nk          = Nk,
#                     Diffk       = Diffk,
#                     dif         = dif,
#                     method      = method,
#                     comparisons = comparisons,
#                     seed        = seed)
#
#   # Save to file if required
#   if (save.to.file){
#     # Get folder
#     if(!dir.exists(folder)) dir.create(folder)
#
#     # Get a unique filename
#     filename <- paste0(folder, "/",
#                        instance$alias,
#                        ".rds")
#
#     # save output to file
#     saveRDS(output, file = filename)
#   }
#
#   # Return output
#   return(output)
# }
















#' Run a full experiment for comparing multiple algorithms using multiple
#' instances
#'
#' Design and run a full experiment - calculate the required number of
#' instances, run the algorithms on each problem instance using the iterative
#' approach based on optimal sample size ratios, and return the results of the
#' experiment. This routine builds upon [calc_instances()] and [calc_nreps()],
#' so refer to the documentation of these two functions for details.
#'
#' @section Instance List:
#' Parameter `instances` must contain a list of instance objects, where
#' each field is itself a list, as defined in the documentation of function
#' [calc_nreps()]. In short, each element of `instances` is an `instance`, i.e.,
#' a named list containing all relevant parameters that define the problem
#' instance. This list must contain at least the field `instance$FUN`, with the
#' name of the problem instance function, that is, a routine that calculates
#' y = f(x). If the instance requires additional parameters, these must also be
#' provided as named fields.
#' An additional field, "instance$alias", can be used to provide the instance
#' with a unique identifier (e.g., when using an instance generator).
#'
#' @section Algorithm List:
#' Object `algorithms` is a list in which each component is a named
#' list containing all relevant parameters that define an algorithm to be
#' applied for solving the problem instance. In what follows `algorithms[[k]]`
#' refers to any algorithm specified in the `algorithms` list.
#'
#' `algorithms[[k]]` must contain an `algorithms[[k]]$FUN` field, which is a
#' character object with the name of the function that calls the algorithm; as
#' well as any other elements/parameters that `algorithms[[k]]$FUN` requires
#' (e.g., stop criteria, operator names and parameters, etc.).
#'
#' The function defined by the routine `algorithms[[k]]$FUN` must have the
#' following structure: supposing that the list in `algorithms[[k]]` has
#' fields `algorithm[[k]]$FUN = "myalgo"`, `algorithms[[k]]$par1 = "a"` and
#' `algorithms[[k]]$par2 = 5`, then:
#'
#'    \preformatted{
#'          myalgo <- function(par1, par2, instance, ...){
#'                #
#'                # <do stuff>
#'                #
#'                return(results)
#'          }
#'    }
#'
#' That is, it must be able to run if called as:
#'
#'    \preformatted{
#'          # remove '$FUN' and '$alias' field from list of arguments
#'          # and include the problem definition as field 'instance'
#'          myargs          <- algorithm[names(algorithm) != "FUN"]
#'          myargs          <- myargs[names(myargs) != "alias"]
#'          myargs$instance <- instance
#'
#'          # call function
#'          do.call(algorithm$FUN,
#'                  args = myargs)
#'    }
#'
#' The `algorithm$FUN` routine must return a list containing (at
#' least) the performance value of the final solution obtained, in a field named
#' `value` (e.g., `result$value`) after a given run. In general it is easier to
#' write a small wrapper funciton around existing implementations.
#'
#' @section Initial Number of Observations:
#' In the _general case_ the initial number of observations / algorithm /
#' instance (`nstart`) should be relatively high. For the parametric case
#' we recommend 10~15 if outliers are not expected, and 30~40 (at least) if that
#' assumption cannot be made. For the bootstrap approach we recommend using at
#' least 15 or 20. However, if some distributional assumptions can be
#' made - particularly low skewness of the population of algorithm results on
#' the test instances), then `nstart` can in principle be as small as 5 (if the
#' output of the algorithm were known to be normal, it could be 1).
#'
#' In general, higher sample sizes are the price to pay for abandoning
#' distributional assumptions. Use lower values of `nstart` with caution.
#'
#' @section Pairwise Differences:
#' Parameter `dif` informs the type of difference in performance to be used
#' for the estimation (\eqn{\mu_a} and \eqn{\mu_b} represent the mean
#' performance of any two algorithms on the test instance, and \eqn{mu}
#' represents the grand mean of all algorithms given in `algorithms`):
#'
#' - If `dif == "perc"` and `comparisons == "all.vs.first"`, the estimated
#' quantity is:
#'    \eqn{\phi_{1b} = (\mu_1 - \mu_b) / \mu_1 = 1 - (\mu_b / \mu_1)}.
#'
#' - If `dif == "perc"` and `comparisons == "all.vs.all"`, the estimated
#' quantity is:
#'    \eqn{\phi_{ab} = (\mu_a - \mu_b) / \mu}.
#'
#' - If `dif == "simple"` it estimates \eqn{\mu_a - \mu_b}.
#'
#' @section Sample Sizes for Nonparametric Methods:
#' If the parameter `` is set to either `Wilcoxon` or `Binomial`, this
#' routine approximates the number of instances using the ARE of these tests
#' in relation to the paired t.test:
#'   - `n.wilcox = n.ttest / 0.86 = 1.163 * n.ttest`
#'   - `n.binom = n.ttest / 0.637 = 1.570 * n.ttest`
#'
#' @inheritParams calc_nreps
#' @inheritParams calc_instances
#' @param instances list object containing the definitions of the
#'    _available_ instances. This list may (or may not) be exhausted in the
#'    experiment. To estimate the number of required instances,
#'    see [calc_instances()]. For more details, see Section `Instance List`.
#' @param power (desired) test power. See [calc_instances()] for details.
#'    Any value equal to or greater than one will force the method to use all
#'    available instances in `Instance.list`.
#' @param d minimally relevant effect size (MRES), expressed as a standardized
#'        effect size, i.e., "deviation from H0" / "standard deviation".
#'        See [calc_instances()] for details.
#' @param sig.level family-wise significance level (alpha) for the experiment.
#'        See [calc_instances()] for details.
#' @param alternative type of alternative hypothesis ("two.sided" or
#'        "one.sided"). See [calc_instances()] for details.
#' @param save.partial.results logical, should partial results be saved to file?
#' @param load.partial.results logical, should previously saved partial results
#'                             be reloaded as part of the experiment?
#'
#' @return a list object containing the following fields:
#' \itemize{
#'    \item \code{Configuration} - the full input configuration (for reproducibility)
#'    \item \code{data.raw} - data frame containing all observations generated
#'    \item \code{data.summary} - data frame summarizing the experiment.
#'    \item \code{N} - number of instances sampled
#'    \item \code{N.star} - number of instances required
#'    \item \code{total.runs} - total number of algorithm runs performed
#'    \item \code{instances.sampled} - names of the instances sampled
#'    \item \code{Underpowered} - flag: TRUE if N < N.star
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#' @references
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms. Journal of Heuristics 25(2):305-338, 2019.
#' - P. Mathews.
#'    Sample size calculations: Practical methods for engineers and scientists.
#'    Mathews Malnar and Bailey, 2010.
#' -  A.C. Davison, D.V. Hinkley:
#'    Bootstrap methods and their application. Cambridge University Press (1997)
#' -  E.C. Fieller:
#'     Some problems in interval estimation. Journal of the Royal Statistical
#'     Society. Series B (Methodological) 16(2), 175–185 (1954)
#' - V. Franz:
#'    Ratios: A short guide to confidence limits and proper use (2007).
#'    https://arxiv.org/pdf/0710.2024v1.pdf
#' - D.C. Montgomery, C.G. Runger:
#'    Applied Statistics and Probability for Engineers, 6th ed. Wiley (2013)
#' - D.J. Sheskin:
#'    Handbook of Parametric and Nonparametric Statistical Procedures,
#'    4th ed., Chapman & Hall/CRC, 1996.
#'
#'
#' @examples
#' \dontrun{
#' # Example using four dummy algorithms and 100 dummy instances.
#' # See [dummyalgo()] and [dummyinstance()] for details.
#' # Generating 4 dummy algorithms here, with means 15, 10, 30, 15 and standard
#' # deviations 2, 4, 6, 8.
#' algorithms <- mapply(FUN = function(i, m, s){
#'   list(FUN   = "dummyalgo",
#'        alias = paste0("algo", i),
#'        distribution.fun  = "rnorm",
#'        distribution.pars = list(mean = m, sd = s))},
#'   i = c(alg1 = 1, alg2 = 2, alg3 = 3, alg4 = 4),
#'   m = c(15, 10, 30, 15),
#'   s = c(2, 4, 6, 8),
#'   SIMPLIFY = FALSE)
#'
#' # Generate 100 dummy instances with centered exponential distributions
#' instances <- lapply(1:100,
#'                     function(i) {rate <- runif(1, 1, 10)
#'                                  list(FUN   = "dummyinstance",
#'                                       alias = paste0("Inst.", i),
#'                                       distr = "rexp", rate = rate,
#'                                       bias  = -1 / rate)})
#'
#' my.results <- run_experiment(instances, algorithms,
#'                              d = .5, se.max = .1,
#'                              power = .9, sig.level = .05,
#'                              power.target = "mean",
#'                              dif = "perc", comparisons = "all.vs.all",
#'                              seed = 1234)
#'
#' # Take a look at the results
#' summary(my.results)
#' print(my.results)
#'}
#'

# run_experiment_old <- function(instances, algorithms, d, se.max,
#                            power = 0.8, sig.level = 0.05,
#                            power.target = "mean",
#                            dif = "simple", comparisons = "all.vs.all",
#                            alternative = "two.sided", test = "t.test",
#                            method = "param",
#                            nstart = 20, nmax = 100 * length(algorithms),
#                            force.balanced = FALSE,
#                            ncpus = 2, boot.R = 499, seed = NULL,
#                            save.partial.results = FALSE,
#                            load.partial.results = FALSE,
#                            folder = "./nreps_files")
# {
#
#   # TODO:
#   # save/load.partial.results can be either a folder, a vector of
#   # file names, or NULL
#   # If it is a folder, then filenames are generated based on instance aliases
#   #
#   # The call to calc_nreps will need to be changed from lapply to mapply
#
#   # ======== Most error catching to be performed by specific routines ======== #
#   assertthat::assert_that(assertthat::is.count(ncpus),
#                           is.null(seed) || seed == seed %/% 1)
#   if (alternative == "one.sided"){
#     assertthat::assert_that(comparisons == "all.vs.first")
#   }
#
#   # Fix a common mistake
#   if (tolower(dif) == "percent") dif <- "perc"
#
#   # Capture input parameters
#   var.input.pars <- as.list(environment())
#
#   # set PRNG seed
#   if (is.null(seed)) {
#     if (!exists(".Random.seed")) stats::runif(1)
#     seed <- .Random.seed #i.e., do not change anything
#   } else {
#     set.seed(seed)
#   }
#
#
#   # Set up parallel processing
#   if ((.Platform$OS.type == "windows") & (ncpus > 1)){
#     cat("\nAttention: multicore not currently available for Windows.\n
#         Forcing ncpus = 1.")
#     ncpus <- 1
#   } else {
#     available.cores <- parallel::detectCores()
#     if (ncpus >= available.cores){
#       cat("\nAttention: ncpus too large, we only have ", available.cores,
#           " cores.\nUsing ", available.cores - 1,
#           " cores for run_experiment().")
#       ncpus <- available.cores - 1
#     }
#   }
#
#   # Fill up algorithm and instance aliases if needed
#   for (i in 1:length(instances)){
#     if (!("alias" %in% names(instances[[i]]))) {
#       instances[[i]]$alias <- instances[[i]]$FUN
#     }
#   }
#   for (i in 1:length(algorithms)){
#     if (!("alias" %in% names(algorithms[[i]]))) {
#       algorithms[[i]]$alias <- algorithms[[i]]$FUN
#     }
#   }
#
#   # Calculate N*
#   n.available   <- length(instances)
#   n.algs        <- length(algorithms)
#   n.comparisons <- switch(comparisons,
#                           all.vs.all = n.algs * (n.algs - 1) / 2,
#                           all.vs.first = n.algs - 1)
#
#   ss.calc <- calc_instances(ncomparisons = n.comparisons,
#                             d            = d,
#                             power        = power,
#                             sig.level    = sig.level,
#                             alternative  = alternative,
#                             test         = test,
#                             power.target = power.target)
#   if (power >= 1) {
#     N.star <- n.available
#   } else {
#     N.star <- ss.calc$ninstances
#     if (N.star < n.available){
#       # Randomize order of presentation for available instances
#       instances <- instances[sample.int(n.available)]
#     }
#   }
#
#   # Echo some information for the user
#   cat("CAISEr running")
#   cat("\n-----------------------------")
#   cat("\nRequired number of instances:", N.star)
#   cat("\nAvailable number of instances:", n.available)
#   cat("\nUsing", ncpus, "cores.")
#   cat("\n-----------------------------")
#
#   # Sample instances
#   if(ncpus > 1){
#     my.results <- pbmcapply::pbmclapply(X = instances[1:min(N.star, n.available)],
#                                         FUN            = calc_nreps,
#                                         # Arguments for calc_nreps:
#                                         algorithms     = algorithms,
#                                         se.max         = se.max,
#                                         dif            = dif,
#                                         comparisons    = comparisons,
#                                         method         = method,
#                                         nstart         = nstart,
#                                         nmax           = nmax,
#                                         boot.R         = boot.R,
#                                         force.balanced = force.balanced,
#                                         load.file      = NULL,
#                                         save.file      = NULL,
#                                         # other pbmclapply arguments:
#                                         mc.cores       = ncpus)
#   } else {
#     my.results <- lapply(X = instances[1:min(N.star, n.available)],
#                          FUN            = calc_nreps,
#                          # Arguments for calc_nreps:
#                          algorithms     = algorithms,
#                          se.max         = se.max,
#                          dif            = dif,
#                          comparisons    = comparisons,
#                          method         = method,
#                          nstart         = nstart,
#                          nmax           = nmax,
#                          boot.R         = boot.R,
#                          force.balanced = force.balanced,
#                          save.to.file   = save.partial.results,
#                          load.from.file = load.partial.results,
#                          folder         = folder)
#   }
#   # Consolidate raw data
#   data.raw <- lapply(X   = my.results,
#                      FUN = function(x){
#                        inst  <- x$instance
#                        nj    <- sum(x$Nk)
#                        data.frame(Algorithm = do.call(what = c,
#                                                       mapply(rep,
#                                                              names(x$Nk),
#                                                              x$Nk,
#                                                              SIMPLIFY = FALSE)),
#                                   Instance    = rep(inst, nj),
#                                   Observation = do.call(c, x$Xk))})
#
#   data.raw <- do.call(rbind, data.raw)
#   rownames(data.raw) <- NULL
#
#   # Consolidate summary data
#   data.summary <- lapply(X   = my.results,
#                          FUN = function(x){
#                            cbind(Instance = rep(x$instance, nrow(x$Diffk)),
#                                  x$Diffk)})
#
#   data.summary <- do.call(rbind, data.summary)
#   algonames <- sapply(algorithms, function(x) x$alias)
#   rownames(data.summary) <- NULL
#   data.summary$Alg1 <- as.factor(algonames[data.summary$Alg1])
#   data.summary$Alg2 <- as.factor(algonames[data.summary$Alg2])
#
#
#   # Assemble output
#   output <- list(Configuration     = var.input.pars,
#                  data.raw          = data.raw,
#                  data.summary      = data.summary,
#                  N                 = min(N.star, n.available),
#                  N.star            = N.star,
#                  total.runs        = nrow(data.raw),
#                  instances.sampled = unique(data.raw$Instance),
#                  Underpowered      = (N.star > n.available),
#                  samplesize.calc   = ss.calc)
#
#   class(output) <- c("CAISEr", "list")
#
#   return(output)
# }

