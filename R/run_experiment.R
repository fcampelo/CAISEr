#' Run a full experiment
#'
#' Design and run a full experiment - calculate the required number of
#' instances, run the algorithms on each problem instance using the iterative
#' approach based on optimal sample size ratios, and return the results of the
#' experiment. This routine builds upon [calc_instances()] and [calc_nreps2()],
#' so refer to the documentation of these two functions for details.
#'
#' @section Instance List:
#' Parameter `Instance.list` must contain a list of instance objects, where
#' each field is itself a list, as defined in Section _Instances and Algorithms
#' of the documentation of _[calc_nreps2()]. In summary, each element of
#' `Instance.list` is an `instance`, i.e., a named list containing all relevant
#' parameters that define the problem instance. This list must contain at least
#' the field `instance$FUN`, with the name of the problem instance function,
#' that is, a routine that calculates y = f(x). If the instance requires
#' additional parameters, these must also be provided as named fields.
#' An additional field, "instance$alias", can be used to
#' provide the instance with a unique identifier (e.g., when using an
#' instance generator).
#'
#' @section Algorithms:
#' Parameter `Algorithm.list` must contain a list of instance objects, where
#' each field is itself a list, as defined in Section _Instances and Algorithms_
#' of the documentation of [calc_nreps2()]. In summary, each element of
#' `Algorithm.list` is an `algorithm`, i.e., a named list containing all
#' relevant parameters that define the algorithm.
#'
#' An `algorithm` must contain a `algorithm$FUN` field (the name
#' of the function that calls the algorithm) and any other elements/parameters
#' that `algorithm$FUN` requires (e.g., stop criteria, operator names and
#' parameters, etc.). An additional field, `algorithm$alias`, can be used to
#' provide the algorithm with a unique identifier (e.g., when comparing two
#' different configurations of the same algorithm).
#'
#' The function defined by the routine `algorithm$FUN` must have the
#' following structure: supposing that the list in `algorithm` has
#' fields `algorithm$FUN = myalgo` and
#' `algorithm$par1 = "a", algorithm$par2 = 5`, then:
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
#'          # remove '$FUN' and '$alias' from list of arguments
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
#' The `algorithm$FUN` routine must return a list object containing (at
#' least) the performance value of the final solution obtained after a given
#' run, in a field named `value` (e.g., `result$value`) .
#'
#' @section Initial Number of Observations:
#' In the _general case_ the initial number of observations / algorithm /
#' instance (`nstart`) should be relatively high. For the parametric case
#' we recommend ~15 if outliers are not expected, ~50 (at least) if that
#' assumption cannot be made. For the bootstrap approch we recommend using at
#' least 15 or 20. However, if some distributional assumptions can be
#' made - particularly low skewness of the population of algorithm results on
#' the test instances), then `nstart` can in principle be as small as 5 (if the
#' output of the algorithm were known to be normal, it could be 1).
#'
#' In general, higher sample sizes are the price to pay for abandoning
#' distributional assumptions. Use lower values of `nstart` with caution.
#'
#' @section Types of Differences:
#' Parameter `dif` informs the type of difference in performance to be used
#' for the estimation (mu1 and mu2 represent the mean performance of each
#' algorithm on the problem instance):
#'
#' - If `dif == "perc"` it estimates (mu2 - mu1) / mu1.
#' - If `dif == "simple"` it estimates mu2 - mu1.
#'
#' @section Sample Sizes for Nonparametric Methods:
#' If the parameter `test.type` is set to either `Wilcoxon` or `Binomial`, this
#' routine approximates the number of instances using the ARE of these tests
#' in relation to the paired t.test, using the formulas:
#'   - `n.wilcox = n.ttest / 0.86 = 1.163 * n.ttest`
#'   - `n.binom = n.ttest / 0.637 = 1.570 * n.ttest`
#'
#' @param Instance.list list object containing the definitions of the
#'    _available_ instances. this list may (or may not) be exhausted in the
#'    experiment. To estimate the number of required instances,
#'    see [calc_instances()]. For more detail on the definition of each
#'    instance, see [calc_nreps2()].
#' @param Algorithm.list list object containing the definitions of the
#'    algorithms to be compared. See [calc_nreps2()] for details.
#' @param power (desired) test power. See [calc_instances()] for details.
#'    Any value equal to or greater than one will force the method to use all
#'    available instances in `Instance.list`.
#' @param d minimally relevant effect size (MRES), expressed as a standardized
#'        effect size, i.e., "deviation from H0" / "standard deviation".
#'        See [calc_instances()] for details.
#' @param sig.level significance level (alpha) for the experiment.
#'        See [calc_instances()] for details.
#' @param alternative type of alternative hypothesis ("two.sided" or
#'        "one.sided"). See [calc_instances()] for details.
#' @param test.type type of test ("t.test", "wilcoxon", "binomial").
#'        See [calc_instances()] for details.
#' @param se.max desired upper limit for the standard error of the estimated
#'        difference between the two algorithms on each instance.
#'        See [calc_nreps2()] for details.
#' @param dif type of difference to be used on each instance. Accepts "perc"
#'        (for percent differences) or "simple" (for simple differences).
#'        See [calc_nreps2()] for details.
#' @param method method to use for estimating the standard errors. Accepts
#'        "param" (for parametric) or "boot" (for bootstrap).
#'        See [calc_nreps2()] for details.
#' @param nstart initial number of algorithm runs for each algorithm in each
#'        instance. See [calc_nreps2()] for details.
#' @param nmax maximum total allowed sample size in each instance
#'        See [calc_nreps2()] for details.
#' @param seed seed for the random number generator
#' @param boot.R number of bootstrap resamples. See [calc_nreps2()] for details.
#' @param force.balanced logical flag to force the use of balanced sampling for
#'        the algorithms on each instance
#' @param ncpus number of cores to use
#' @param save.partial.results logical flag: should individual instance results
#'        be saved to file?
#'
#' @return a list object containing the full input configuration plus the
#' following fields:
#' \itemize{
#'    \item \code{data.raw} - data frame containing all observations generated
#'    \item \code{data.summary} - data frame summarizing the experiment.
#'    \item \code{N} - number of instances sampled
#'    \item \code{N.star} - number of instances required
#'    \item \code{instances.sampled} - names of the instances sampled
#'    \item \code{Underpowered} - flag: TRUE if N < N.star
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @references
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms (submitted, 2017).
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
#' # In this case all instances are the same, so we expect all cases to return
#' # a percent difference of approx. phi.j = 1.0 and sample sizes of
#' # approx. n1 = 31, n2 = 87
#' algorithm1 <- list(FUN = "dummyalgo", alias = "algo1",
#'                    distribution.fun = "rnorm",
#'                    distribution.pars = list(mean = 10, sd = 1))
#' algorithm2 <- list(FUN = "dummyalgo", alias = "algo2",
#'                    distribution.fun = "rnorm",
#'                    distribution.pars = list(mean = 20, sd = 4))
#' algolist <- list(algorithm1, algorithm2)
#' instlist <- vector(100, mode = "list")
#' for (i in 1:100) instlist[[i]] <- list(FUN = "dummyinstance",
#'                                        alias = paste0("Inst. ", i))
#'
#' my.results <- run_experiment(Instance.list = instlist,
#'                              Algorithm.list = algolist,
#'                              power = 0.8,
#'                              d = 1,
#'                              sig.level = 0.01,
#'                              se.max = 0.05,
#'                              dif = "perc",
#'                              nmax   = 200,
#'                              seed   = 1234,
#'                              ncpus  = 2)
#'
#' # Take a look at the summary table
#' my.results$data.summary
#'
#' # To perform inference on the results:
#' t.test(my.results$data.summary$phi.j, conf.level = 0.95)
#'
#' # Test assumption of normality (of the data)
#' shapiro.test(my.results$data.summary$phi.j)

# @param ncpus number of cores to use. See [calc_nreps2()] for details. #//DoParallel

# TESTED
run_experiment <- function(Instance.list,    # instance parameters
                           Algorithm.list,   # algorithm parameters
                           power ,           # power
                           d,                # MRES
                           sig.level = 0.05,          # significance level
                           alternative = "two.sided", # type of H1
                           test.type = "t.test",      # type of test
                           se.max,           # desired (max) standard error
                           dif,              # difference ("simple", "perc"),
                           method = "param", # method ("param", "boot")
                           nstart = 20,      # initial number of samples
                           nmax   = 1000,    # maximum allowed sample size
                           seed   = NULL,    # seed for PRNG
                           boot.R = 999,     # number of bootstrap resamples
                           force.balanced = FALSE, # force balanced sampling
                           ncpus  = 1,       # number of cores to use
                           save.partial.results = FALSE) # save tmp files?
{

  # ========== Error catching to be performed by specific routines ========== #

  # Capture input parameters
  var.input.pars <- as.list(sys.call())[-1]

  # set PRNG seed
  if (is.null(seed)) {
    if (!exists(".Random.seed")) stats::runif(1)
    seed <- .Random.seed #i.e., do not change anything
  } else {
    set.seed(seed)
  }


  # Set up parallel processing
  if ((.Platform$OS.type == "windows") & (ncpus > 1)){
    cat("\nAttention: multicore not available for Windows.\n
        Forcing ncpus = 1.")
    ncpus <- 1
  } else {
    available.cores <- parallel::detectCores()
    if (ncpus >= available.cores){
      cat("\nAttention: ncpus too large, we only have ", available.cores,
          " cores.\nUsing ", available.cores - 1,
          " cores for run_experiment().")
      ncpus <- available.cores - 1
    }
  }

  # Fill up algorithm and instance aliases if needed
  for (i in 1:length(Instance.list)){
    if (!("alias" %in% names(Instance.list[[i]]))) {
      Instance.list[[i]]$alias <- Instance.list[[i]]$FUN
    }
  }

  # Calculate N*
  n.available <- length(Instance.list)

  if (power >= 1) {
    N.star <- n.available
  } else {
    ninstances <- calc_instances(power       = power,
                                 d           = d,
                                 sig.level   = sig.level,
                                 alternative = alternative,
                                 test.type   = test.type)
    N.star <- ninstances$ninstances

    # Randomize order of presentation for available instances
    Instance.list <- Instance.list[sample.int(n.available)]
  }

  # Echo some information for the user
  cat("CAISEr running")
  cat("\n-----------------------------")
  cat("\nRequired number of instances:", N.star)
  cat("\nAvailable number of instances:", n.available)
  cat("\n-----------------------------")

  # Initialize output structures
  data.raw <- data.frame(Algorithm    = character(0),
                         Instance     = character(0),
                         Observation  = numeric(0))
  data.summary <- data.frame(Instance = character(0),
                             phi.j    = numeric(0),
                             std.err  = numeric(0),
                             n1j      = numeric(0),
                             n2j      = numeric(0))

  # Sample instances
  my.results <- pbmcapply::pbmclapply(X = Instance.list[1:min(N.star,
                                                              n.available)],
                                      FUN            = calc_nreps2,
                                      algorithm1     = Algorithm.list[[1]],
                                      algorithm2     = Algorithm.list[[2]],
                                      se.max         = se.max,
                                      dif            = dif,
                                      method         = method,
                                      nstart         = nstart,
                                      nmax           = nmax,
                                      boot.R         = boot.R,
                                      force.balanced = force.balanced,
                                      save.to.file   = save.partial.results,
                                      mc.cores       = ncpus)

  # Consolidate raw data
  data.raw <- lapply(X   = seq(length(my.results)),
                     FUN = function(j, my.results, alias1, alias2, inst.list){
                       res_j <- my.results[[j]]
                       inst  <- inst.list[[j]]$alias
                       nj    <- res_j$n1j + res_j$n2j
                       data.frame(Algorithm   = c(rep(alias1, res_j$n1j),
                                                  rep(alias2, res_j$n2j)),
                                  Instance    = rep(inst, nj),
                                  Observation = c(res_j$x1j, res_j$x2j),
                                  stringsAsFactors = FALSE)},
                     my.results = my.results,
                     alias1     = Algorithm.list[[1]]$alias,
                     alias2     = Algorithm.list[[2]]$alias,
                     inst.list  = Instance.list)

  data.raw <- do.call(rbind, data.raw)

  # Consolidate summary data
  data.summary <- lapply(X   = seq(length(my.results)),
                         FUN = function(j, my.results, inst.list){
                           data.frame(Instance = inst.list[[j]]$alias,
                                      phi.j    = my.results[[j]]$phi.est,
                                      std.err  = my.results[[j]]$se,
                                      n1j      = my.results[[j]]$n1j,
                                      n2j      = my.results[[j]]$n2j,
                                      stringsAsFactors = FALSE)},
                         my.results = my.results,
                         inst.list  = Instance.list)

  data.summary <- do.call(rbind, data.summary)

  # Assemble output
  output <- list(Configuration     = var.input.pars,
                 data.raw          = data.raw,
                 data.summary      = data.summary,
                 N                 = min(N.star, n.available),
                 N.star            = N.star,
                 instances.sampled = unique(data.summary$Instance),
                 Underpowered      = (N.star > n.available))

  class(output) <- c("CAISEr", "list")

  return(output)
}

