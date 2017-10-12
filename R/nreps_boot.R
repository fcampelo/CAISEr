#' Determine sample sizes for a pair of algorithms on a problem instance
#'
#' Iteratively calculates the required sample sizes for two algorithms
#' on a given problem instance using the **bootstrap** approach
#' (based on normality of the sampling distribution of the means)
#' so that the standard error
#' of the estimate of the difference (either simple or percent) in mean
#' performance is controlled at a predefined level.
#'
#' @section Instances and Algorithms:
#' Parameters `instance`, `algorithm1` and `algorithm2` must each
#' be a list of instance (algorithm) specifications, defined according to the
#' instructions given below.
#'
#' `instance` is a named list containing all relevant parameters that
#' define the problem instance. This list must contain at least the field
#' `instance$name`, with the name of the problem instance function, that is, a
#' routine that calculates y = f(x). If the instance requires additional
#' parameters, these must also be provided as named fields.
#'
#' Similarly, `algorithm1` and `algorithm2` must each be a named list
#' containing all relevant parameters that define the algorithm to be applied
#' for solving the problem instance. In what follows we use `algorithm` to
#' refer to both `algorithm1` and `algorithm2`
#'
#' `algorithm` must contain a `algorithm$name` field (the name
#' of the function that calls the algorithm) and any other elements/parameters
#' that `algorithm$name` requires (e.g., stop criteria, operator names and
#' parameters, etc.).
#'
#' The function defined by the routine `algorithm$name` must have the
#' following structure: supposing that the list in `algorithm` has
#' fields `algorithm$name = myalgo` and
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
#'          # remove '$name' field from list of arguments
#'          # and include the problem definition as field 'instance'
#'          myargs          <- algorithm[names(algorithm) != "name"]
#'          myargs$instance <- instance
#'
#'          # call function
#'          do.call(algorithm$name,
#'                  args = myargs)
#'    }
#'
#' The `algorithm$name` routine must return a list containing (at
#' least) the performance value of the final solution obtained, in a field named
#' `value` (e.g., `result$value`) after a given run.
#'
#' @section Initial Number of Observations:
#' In the **general case** the initial number of observations / algorithm /
#' instance (`nstart`) should be relatively high. For the parametric case
#' we recommend ~20 if outliers are not expected, ~50 (at least) if that
#' assumption cannot be made. For the bootstrap approch we recommend using at
#' least 20. However, if some distributional assumptions can be
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
#' @param instance a list object containing the definitions of the problem
#'    instance.
#'    See Section _Problems and Algorithms_ for details.
#' @param algorithm1 a list object containing the definitions of algorithm 1.
#'    See Section _Problems and Algorithms_ for details.
#' @param algorithm2 a list object containing the definitions of algorithm 2.
#'    See Section _Problems and Algorithms_ for details.
#' @param se.max desired upper limit for the standard error of the estimated
#'        difference between the two algorithms. See Section
#'        _Types of Differences_ for details.
#' @param dif name of the difference for which the SE is desired. Accepts "perc"
#'          (for percent differences) or "simple" (for simple differences)
#' @param nstart initial number of algorithm runs for each algorithm.
#'      See Section _Initial Number of Observations_ for details.
#' @param nmax maximum total allowed sample size.
#' @param seed seed for the random number generator
#' @param boot.R number of bootstrap resamples
#' @param ncpus number of cores to use
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{x1j} - vector of observed performance values for `algorithm1`
#'    \item \code{x2j} - vector of observed performance values for `algorithm2`
#'    \item \code{phi.est} - estimated value for the statistic of interest
#'    \item \code{se} - standard error of the estimate
#'    \item \code{n1j} - number of observations generated for algorithm 1
#'    \item \code{n2j} - number of observations generated for algorithm 2
#'    \item \code{r.opt = n1j / n2j}
#'    \item \code{seed} - the seed used for the PRNG
#'    \item \code{dif} - the type of difference used
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Fernanda Takahashi (\email{fernandact@@ufmg.br})
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
#'
#' @export

nreps_boot <- function(instance,         # instance parameters
                       algorithm1,       # algorithm parameters
                       algorithm2,       # algorithm parameters
                       se.max,           # desired (max) standard error
                       dif,              # difference ("simple", "perc"),
                       nstart = 20,      # initial number of samples
                       nmax   = Inf,     # maximum allowed sample size
                       seed   = NULL,    # seed for PRNG
                       boot.R = 999,     # number of bootstrap resamples
                       ncpus  = 1)       # number of cores to use
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.list(instance),
    assertthat::has_name(instance, "name"),
    is.list(algorithm1), is.list(algorithm2),
    assertthat::has_name(algorithm1, "name"),
    assertthat::has_name(algorithm2, "name"),
    is.numeric(se.max) && length(se.max) == 1,
    dif %in% c("simple", "perc"),
    assertthat::is.count(nstart),
    is.infinite(nmax) || assertthat::is.count(nmax),
    nmax >= 2 * nstart,
    is.null(seed) || assertthat::is.count(seed),
    assertthat::is.count(boot.R), boot.R > 1,
    assertthat::is.count(ncpus))
  # ==================================== #

  # set PRNG seed
  if (is.null(seed)) {
    seed <- .Random.seed #i.e., do not change anything
  }
  set.seed(seed)

  # Set up doParallel
  local.cluster <- FALSE
  if (ncpus > 1){
    cl.workers <- getDoParWorkers()
    if (cl.workers < ncpus){
      available.cores <- parallel::detectCores()
      if (ncpus >= available.cores){
        warning("ncpus too large, we only have ", available.cores, " cores. ",
                "Using ", ncores - 1, " cores.")
        ncpus <- available.cores - 1
      }
      if (cl.workers < ncpus){
        cl.nreps_boot <- parallel::makeCluster(ncpus)
        doParallel::registerDoParallel(cl.nreps_boot)
        local.cluster <- TRUE
      }
    }
  }

  # generate initial samples and estimates of improvement for each algorithm
  x1j      <- get_observations(algorithm1, instance, nstart - 1)
  x2j      <- get_observations(algorithm2, instance, nstart - 1)
  SE       <- calc_se(x1 = x1j, x2 = x2j, dif = dif,
                      method = "boot", boot.R = boot.R)

  x1j      <- c(x1j, get_observations(algorithm1, instance, 1))
  SE.prime <- calc_se(x1 = x1j, x2 = x2j, dif = dif,
                      method = "boot", boot.R = boot.R)
  delta1   <- SE$se - SE.prime$se
  n1j      <- nstart

  x2j      <- c(x2j, get_observations(algorithm2, instance, 1))
  SE       <- calc_se(x1 = x1j, x2 = x2j, dif = dif,
                      method = "boot", boot.R = boot.R)
  delta2   <- SE.prime$se - SE$se
  n2j      <- nstart


  while(SE$se > se.max & (n1j + n2j) < nmax){
    SE.prime <- SE
    if (delta1 > delta2){                 # sample algorithm 1
      x1j      <- c(x1j, get_observations(algorithm1, instance, 1))
      SE       <- calc_se(x1 = x1j, x2 = x2j, dif = dif,
                          method = "boot", boot.R = boot.R)
      delta1   <- SE.prime$se - SE$se
      n1j      <- n1j + 1
    } else{                               # sample algorithm 2
      x2j      <- c(x2j, get_observations(algorithm2, instance, 1))
      SE       <- calc_se(x1 = x1j, x2 = x2j, dif = dif,
                          method = "boot", boot.R = boot.R)
      delta2   <- SE.prime$se - SE$se
      n2j      <- n2j + 1
    }
  }

  # unregister cluster if needed
  if (local.cluster) { stopCluster(cl.nreps_boot) }

  output <- list(x1j     = x1j,
                 x2j     = x2j,
                 phi.est = SE$x.est,
                 se      = SE$se,
                 n1j     = n1j,
                 n2j     = n2j,
                 r.opt   = n1j / n2j,
                 seed    = seed,
                 dif     = dif)

  return(output)
}
