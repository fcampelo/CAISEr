#' Determine sample sizes for a pair of algorithms on a problem instance
#'
#' Iteratively calculates the required sample sizes for two algorithms
#' on a given problem instance, so that the standard error
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
#' @inheritParams calc_se
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
#' @param nstart initial number of algorithm runs for each algorithm.
#'      See Section _Initial Number of Observations_ for details.
#' @param nmax maximum total allowed sample size.
#' @param seed seed for the random number generator
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{x1} - vector of observed performance values for `algorithm1`
#'    \item \code{x2} - vector of observed performance values for `algorithm2`
#'    \item \code{phi.est} - estimated value for the statistic of interest of
#'          the difference of performance of `algorithm1` and `algorithm2` on
#'          `instance`
#'    \item \code{se} - standard error of the estimate
#'    \item \code{n} - total number of observations generated
#'    \item \code{seed} - the seed used for the PRNG
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
#' -  A.C. Davison, D.V. Hinkley:
#'    Bootstrap methods and their application. Cambridge University Press (1997)
#' -  E.C. Fieller:
#'     Some problems in interval estimation. Journal of the Royal Statistical
#'     Society. Series B (Methodological) 16(2), 175–185 (1954)
#' - V. Franz:
#'    Ratios: A short guide to confidence limits and proper use (2007).
#'    https://arxiv.org/pdf/0710.2024v1.pdf
#' - D.C. Montgomery, C.G. Runger:
#'    Applied Statistics and Probability for Engineers, 6th edn. Wiley (2013)
#'
#' @export

nreps_boot <- function(instance,         # instance parameters
                        algorithm1,       # algorithm parameters
                        algorithm2,       # algorithm parameters
                        se.max,           # desired (max) standard error
                        method = "param", # technique to calculate CI #method = c("param", "boot"),
                        dif,              # difference #dif   = c("simple", "perc"),
                        nstart = 20,      # initial number of samples
                        nmax   = Inf,     # maximum allowed sample size
                        seed   = NULL,    # seed for PRNG
                        boot.R = 999,     # number of bootstrap
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
    method %in% c("param", "boot"),
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

  # genera
  n1     <- nstart # initial number of observations
  n2     <- nstart # initial number of observations

  opt.rt <- 1    #optmal.ratio
  delta  <- Inf


  # initialize vector of observations
  x1     <- get_observations(algo[[1]], instance, n1)
  x2     <- get_observations(algo[[2]], instance, n2)

  if (method=="boot"){
    SE     <- calc_se(x1,x2,alpha = alpha, dif=dif, method = method, ...)
    x1     <- c(x1, get_observations(algo[[1]], instance, 1))
    SE_    <- calc_se(x1,x2,alpha = alpha, dif=dif, method = method, ...)
    delta1 <- SE$se - SE_$se
    x2     <- c(x2, get_observations(algo[[2]], instance, 1))
    SE     <- calc_se(x1,x2,alpha = alpha, dif=dif, method = method, ...)
    delta2 <- SE_$se - SE$se
    n1 <- nstart
    n2 <- nstart
    delta <- SE$se
  }

  while(delta > dmax && (n1 <= nmax && n2 <= nmax)){
    # when using the bootstrap with percent differences an optmal.ratio is not returned
    # entering the else lace
    # insted of the optmal ratio, the SE funcition returns the algorithm whose the increase
    # of observations led to a greater reduction of the standard error
    if (method == "param"){
      if (opt.rt >= n1/n2){
        n1      <- n1 + 1
        x1      <- c(x1, get_observations(algo[[1]], instance, 1))
      }
      if(opt.rt <= n1/n2){
        n2      <- n2 + 1
        x2      <- c(x2, get_observations(algo[[2]], instance, 1))
      }

      SE     <- calc_se(x1,x2,alpha = alpha, dif=dif, res.boot = boot.info, method = method, ...)
    }else{
      if(method == "boot"){
        SE_ <- SE
        which_run <- 0
        if(delta2 == delta1) which_run <- rbinom(1, 1,0.5)
        else if(delta2 >delta1) which_run <- 1

        if (which_run == 0){
          n1      <- n1 + 1
          x1      <- c(x1, get_observations(algo[[1]], instance, 1))
          SE      <- calc_se(x1,x2,alpha = alpha, dif=dif,  method = method, ...)
          delta1  <- SE_$se - SE$se
        }
        else{
          n2      <- n2 + 1
          x2      <- c(x2, get_observations(algo[[2]], instance, 1))
          SE      <- calc_se(x1,x2,alpha = alpha, dif=dif, method = method, ...)
          delta2 <- SE_$se - SE$se
        }
      }
    }

    delta  <- SE$se
    opt.rt <- SE$opt.rt

  }
  output<-list(x1     = x1,
               x2     = x2,
               x.est  = SE$est,
               x.CI   = SE$ci,
               n1     = n1,
               n2     = n2,
               se     = SE$se,
               delta  = delta,
               seed   = seed,
               dif    = dif)

  return(output)
}
