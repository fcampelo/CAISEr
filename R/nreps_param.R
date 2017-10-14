#' Determine sample sizes for a pair of algorithms on a problem instance
#'
#' Iteratively calculates the required sample sizes for two algorithms
#' on a given problem instance using the **parametric** approach
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
#' `instance$FUN`, with the name of the problem instance function, that is, a
#' routine that calculates y = f(x). If the instance requires additional
#' parameters, these must also be provided as named fields.
#'
#' Similarly, `algorithm1` and `algorithm2` must each be a named list
#' containing all relevant parameters that define the algorithm to be applied
#' for solving the problem instance. In what follows we use `algorithm` to
#' refer to both `algorithm1` and `algorithm2`
#'
#' `algorithm` must contain a `algorithm$FUN` field (the name
#' of the function that calls the algorithm) and any other elements/parameters
#' that `algorithm$FUN` requires (e.g., stop criteria, operator names and
#' parameters, etc.).
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
#'          # remove '$FUN' field from list of arguments
#'          # and include the problem definition as field 'instance'
#'          myargs          <- algorithm[names(algorithm) != "FUN"]
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
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{x1j} - vector of observed performance values for `algorithm1`
#'    \item \code{x2j} - vector of observed performance values for `algorithm2`
#'    \item \code{phi.est} - estimated value for the statistic of interest
#'    \item \code{se} - standard error of the estimate
#'    \item \code{n1j} - number of observations generated for algorithm 1
#'    \item \code{n2j} - number of observations generated for algorithm 2
#'    \item \code{r.opt} - estimated optimal ratio of sample sizes
#'    \item \code{seed} - the seed used for the PRNG
#'    \item \code{dif} - the type of difference used
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Fernanda Takahashi (\email{fernandact@@ufmg.br})
#'
#' @section References:
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms (submitted, 2017).
#' - P. Mathews.
#'    Sample size calculations: Practical methods for engineers and scientists.
#'    Mathews Malnar and Bailey, 2010.
#' -  E.C. Fieller:
#'     Some problems in interval estimation. Journal of the Royal Statistical
#'     Society. Series B (Methodological) 16(2), 175–185 (1954)
#' - V. Franz:
#'    Ratios: A short guide to confidence limits and proper use (2007).
#'    https://arxiv.org/pdf/0710.2024v1.pdf
#' - D.C. Montgomery, C.G. Runger:
#'    Applied Statistics and Probability for Engineers, 6th edn. Wiley (2013)
#'
#' @examples
#' # Example 1: uses dummy algorithms and a dummy instance to illustrate the
#' # use of nreps_param
#' algorithm1 <- list(FUN = "dummyalgo", alias = "algo1",
#'                    distribution.fun = "rnorm",
#'                    distribution.pars = list(mean = 10, sd = 1))
#' algorithm2 <- list(FUN = "dummyalgo", alias = "algo2",
#'                    distribution.fun = "rnorm",
#'                    distribution.pars = list(mean = 20, sd = 4))
#' instance <- list(FUN = "dummyinstance")
#'
#' # Theoretical n1, n2 for an SE = 0.5 on the simple difference: 20, 80
#' my.reps  <- nreps_param(instance, algorithm1, algorithm2,
#'                         se.max = 0.5, dif = "simple", seed = 1234)
#' cat("n1j   =", my.reps$n1j, "\nn2j   =", my.reps$n2j,
#'     "\nphi_j =", my.reps$phi.est, "\nse    =", my.reps$se)
#'
#' # Theoretical n1, n2 for an SE = 0.05 on the percent difference: 31, 87
#' my.reps  <- nreps_param(instance, algorithm1, algorithm2,
#'                         se.max = 0.05, dif = "perc",
#'                         nstart = 10, seed = 1234)
#' cat("n1j   =", my.reps$n1j, "\nn2j   =", my.reps$n2j,
#'     "\nphi_j =", my.reps$phi.est, "\nse    =", my.reps$se)

# TESTED
nreps_param <- function(instance,         # instance parameters
                        algorithm1,       # algorithm parameters
                        algorithm2,       # algorithm parameters
                        se.max,           # desired (max) standard error
                        dif,              # difference ("simple", "perc"),
                        nstart = 20,      # initial number of samples
                        nmax   = 1000,    # maximum allowed sample size
                        seed   = NULL)    # seed for PRNG
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.list(instance),
    assertthat::has_name(instance, "FUN"),
    is.list(algorithm1), is.list(algorithm2),
    assertthat::has_name(algorithm1, "FUN"),
    assertthat::has_name(algorithm2, "FUN"),
    is.numeric(se.max) && length(se.max) == 1,
    dif %in% c("simple", "perc"),
    assertthat::is.count(nstart),
    is.infinite(nmax) || assertthat::is.count(nmax),
    nmax >= 2 * nstart,
    is.null(seed) || assertthat::is.count(seed))
  # ==================================== #

  # set PRNG seed
  if (is.null(seed)) {
    seed <- .Random.seed #i.e., do not change anything
  }
  set.seed(seed)

  # generate initial samples
  n1j <- nstart # initial number of observations
  n2j <- nstart # initial number of observations
  x1j <- get_observations(algorithm1, instance, n1j)
  x2j <- get_observations(algorithm2, instance, n2j)

  SE <- calc_se(x1 = x1j, x2 = x2j,
                dif = dif, method = "param")

  while(SE$se > se.max & (n1j + n2j) < nmax){
    r.opt <- calc_ropt(x1 = x1j, x2 = x2j, dif = dif)
    if (n1j / n2j < r.opt) {   # sample algorithm 1
      x1j <- c(x1j, get_observations(algorithm1, instance, 1))
      n1j <- n1j + 1
    } else {                  # sample algorithm 2
      x2j <- c(x2j, get_observations(algorithm2, instance, 1))
      n2j <- n2j + 1
    }
    SE <- calc_se(x1 = x1j, x2 = x2j,
                  dif = dif, method = "param")
  }

  output <- list(x1j     = x1j,
                 x2j     = x2j,
                 phi.est = SE$x.est,
                 se      = SE$se,
                 n1j     = n1j,
                 n2j     = n2j,
                 r.opt   = calc_ropt(x1 = x1j, x2 = x2j, dif = dif),
                 seed    = seed,
                 dif     = dif)

  return(output)
}
