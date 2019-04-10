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
#' output of the algorithm were known to be normal, it could be 1).
#'
#' In general, higher sample sizes are the price to pay for abandoning
#' distributional assumptions. Use lower values of `nstart` with caution.
#'
#' @section Pairwise Differences:
#' Parameter `dif` informs the type of difference in performance to be used
#' for the estimation (`mu_a` and `mu_b` represent the mean performance of any
#' two algorithms on the test instance, and `mu` represents the grand mean of all
#' algorithms given in `algorithms`):
#'
#' - If `dif == "perc"` and `type == "all.vs.first`, the estimated quantity is
#'    phi_{1b} = (mu_1 - mu_b) / mu_1 = 1 - (mu_b / mu_1).
#' - If `dif == "perc"` and `type == "all.vs.all`, the estimated quantity is
#'    phi_{ab} = (mu_a - mu_b) / mu_a = 1 - (mu_b / mu_a).
#' - If `dif == "simple"` it estimates mu_a - mu_b.
#'
#' @param instance a list object containing the definitions of the problem
#'    instance.
#'    See Section _Problems and Algorithms_ for details.
#' @param algorithms a list object containing the definitions of all algorithms.
#'    See Section _Problems and Algorithms_ for details.
#' @param se.max desired upper limit for the standard error of the estimated
#'        difference between pairs of algorithms. See Section
#'        _Pairwise Differences_ for details.
#' @param dif type of difference to be used. Accepts "perc"
#'          (for percent differences) or "simple" (for simple differences)
#' @param type type of comparisons being performed. Accepts "all.vs.first"
#'          (in which cases the first object in `algorithms` is considered to be
#'          the reference algorithm) or "all.vs.all" (if there is no reference
#'          and all pairwise comparisons are desired).
#' @param method method to use for estimating the standard errors. Accepts
#'          "param" (for parametric) or "boot" (for bootstrap)
#' @param nstart initial number of algorithm runs for each algorithm.
#'      See Section _Initial Number of Observations_ for details.
#' @param nmax maximum **total** allowed sample size.
#' @param seed seed for the random number generator
#' @param boot.R number of bootstrap resamples to use (if `method == "boot"`)
#' @param ncpus number of cores to use
#' @param force.balanced logical flag to force the use of balanced sampling for
#'        the algorithms on each instance
#' @param save.to.file logical flag: should the results be saved to a file
#'        in the current working directory?
#' @param folder directory to save files (if `save.to.file == TRUE`)
#'
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{Xk} - list of observed performance values for all `algorithms`
#'    \item \code{Phik} - estimated values for the statistics of interest
#'    \item \code{SEk} - standard errors of the estimates
#'    \item \code{nk} - vector with the number of observations generated for
#'                      each algorithm
#'    \item \code{seed} - the seed used for the PRNG
#'    \item \code{dif} - the type of difference used
#'    \item \code{method} - the method used ("param" / "boot")
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
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
#' # Uses dummy algorithms and a dummy instance to illustrate the
#' # use of calc_nreps2
#' algorithm1 <- list(FUN = "dummyalgo", alias = "algo1",
#'                    distribution.fun = "rnorm",
#'                    distribution.pars = list(mean = 10, sd = 1))
#' algorithm2 <- list(FUN = "dummyalgo", alias = "algo2",
#'                    distribution.fun = "rnorm",
#'                    distribution.pars = list(mean = 20, sd = 4))
#' algorithm3 <- list(FUN = "dummyalgo", alias = "algo3",
#'                    distribution.fun = "rnorm",
#'                    distribution.pars = list(mean = 30, sd = 10))
#' algorithms <- list(alg1 = algorithm1, alg2 = algorithm2, alg3 = algorithm3)
#' instance <- list(FUN = "dummyinstance")
#'
#' se.max = 0.5
#' dif = "simple"
#' type = "all.vs.all",
#' seed = 1234
#' method = "param"
#' nstart = 20
#' ncpus  = 1
#' nmax   = 200
#' boot.R = 999
#' force.balanced = FALSE
#' save.to.file  = FALSE
#' folder = "./nreps_files"
#'
#'
#'

#UNTESTED
calc_nreps <- function(instance,         # instance parameters
                       algorithms,       # algorithm parameters
                       se.max,           # desired (max) standard error
                       dif,              # difference ("simple", "perc"),
                       method = "param", # method ("param", "boot")
                       nstart = 20,      # initial number of samples
                       nmax   = 200,    # maximum allowed sample size
                       seed   = NULL,    # seed for PRNG
                       boot.R = 999,     # number of bootstrap resamples
                       ncpus  = 1,       # number of cores to use
                       force.balanced = FALSE, # force balanced sampling
                       save.to.file  = FALSE, # save results to tmp file
                       folder = "./nreps_files") # directory to save files (if save.to.file == TRUE)
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.list(instance),
    assertthat::has_name(instance, "FUN"),
    is.list(algorithms),
    all(sapply(X = algorithms, FUN = is.list)),
    all(sapply(X = algorithms,
               FUN = function(x){assertthat::has_name(x, "FUN")})),
    is.numeric(se.max) && length(se.max) == 1,
    dif %in% c("simple", "perc"),
    type %in% c("all.vs.all", "all.vs.first"),
    method %in% c("param", "boot"),
    assertthat::is.count(nstart),
    is.infinite(nmax) || assertthat::is.count(nmax),
    nmax >= length(algorithms) * nstart,
    is.null(seed) || assertthat::is.count(seed),
    assertthat::is.count(boot.R), boot.R > 1,
    is.logical(force.balanced), length(force.balanced) == 1,
    is.logical(save.to.file), length(save.to.file) == 1)
  # ==================================== #

  # set PRNG seed
  if (is.null(seed)) {
    if (!exists(".Random.seed")) stats::runif(1)
    seed <- .Random.seed #i.e., do not change anything
  } else{
    set.seed(seed)
  }

  # Get/set instance alias
  if (!("alias" %in% names(instance))) {
    instance$alias <- instance$FUN
  }

  # Echo some information for the user
  cat("\nSampling algorithms on instance:", instance$alias)

  # generate initial samples
  Nk <- rep(nstart, length(algorithms))
  Xk <- parallel::mcmapply(FUN      = get_observations,
                           algo     = algorithms,
                           n        = Nk,
                           MoreArgs = list(instance = instance),
                           mc.cores = ncpus,
                           SIMPLIFY = FALSE)

  # PAREI AQUI (inside calc_sek)
  SEk <- lapply(X      = Xk,
                FUN    = calc_sek,
                dif    = dif,
                type   = type,
                method = method,
                boot.R = boot.R)

#  SE <- calc_se(x1 = x1j, x2 = x2j,
#                dif = dif, method = method, boot.R = boot.R)

  while(SE$se > se.max & (n1j + n2j) < nmax){
    # Echo something for the user
    if (!(n1j + n2j) %% nstart) cat(".")

    if (force.balanced) {
      x1j <- c(x1j, get_observations(algorithm1, instance, 1))
      x2j <- c(x2j, get_observations(algorithm2, instance, 1))
      n1j <- n1j + 1
      n2j <- n2j + 1
    } else {
      # Calculate optimal ratio
      r.opt <- calc_ropt(x1 = x1j, x2 = x2j, dif = dif)

      # Sample according to r.opt
      if (n1j / n2j < r.opt) {    # sample algorithm 1
        x1j <- c(x1j, get_observations(algorithm1, instance, 1))
        n1j <- n1j + 1
      } else {                    # sample algorithm 2
        x2j <- c(x2j, get_observations(algorithm2, instance, 1))
        n2j <- n2j + 1
      }
    }

    # Recalculate SE
    SE <- calc_se(x1 = x1j, x2 = x2j,
                  dif = dif, method = method, boot.R = boot.R)
  }

  # Assemble output list
  output <- list(instance = instance$alias,
                 x1j      = x1j,
                 x2j      = x2j,
                 phi.est  = SE$x.est,
                 se       = SE$se,
                 n1j      = n1j,
                 n2j      = n2j,
                 seed     = seed,
                 dif      = dif)

  # Save to file if required
  if (save.to.file){
    # Get folder
    if(!dir.exists(folder)) dir.create(folder)

    # Get a unique filename
    filename <- paste0(folder, "/",
                       instance$alias,
                       ".rds")

    # save output to file
    saveRDS(output, file = filename)
  }

  # Return output
  return(output)
}
