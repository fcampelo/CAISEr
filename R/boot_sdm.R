#' Bootstrap the sampling distribution of the mean
#'
#' Bootstraps the sampling distribution of the means for a given vector of observations
#'
#' @section References:
#' -  A.C. Davison, D.V. Hinkley:
#'    Bootstrap methods and their application. Cambridge University Press (1997)
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms. Journal of Heuristics 25(2):305-338, 2019.
#'
#' @param x vector of observations
#' @param boot.R (optional) number of bootstrap resamples
#' #@param ncpus (optional) number of cores to use #//DoParallel
#' @param seed seed for the PRNG
#'
#' @return vector of bootstrap estimates of the sample mean
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @export
#'
#' @examples
#' x <- rnorm(15, mean = 4, sd = 1)
#' my.sdm <- boot_sdm(x)
#' hist(my.sdm)
#' qqnorm(my.sdm, pch = 20)
#'
#' x <- runif(12)
#' my.sdm <- boot_sdm(x)
#' qqnorm(my.sdm, pch = 20)
#'
#' # Convergence of the SDM to a Normal distribution as sample size is increased
#' X <- rchisq(1000, df = 3)
#' x1 <- rchisq(10, df = 3)
#' x2 <- rchisq(20, df = 3)
#' x3 <- rchisq(40, df = 3)
#' par(mfrow = c(2, 2))
#' plot(density(X), main = "Estimated pop distribution");
#' hist(boot_sdm(x1), breaks = 25, main = "SDM, n = 10")
#' hist(boot_sdm(x2), breaks = 25, main = "SDM, n = 20")
#' hist(boot_sdm(x3), breaks = 25, main = "SDM, n = 40")
#' par(mfrow = c(1, 1))

# TESTED
boot_sdm <- function(x,             # vector of observations
                     boot.R = 999,  # number of bootstrap resamples
                     #ncpus  = 1,    # number of cores to use   #//DoParallel
                     seed   = NULL) # PRNG seed
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.numeric(x), length(x) > 1,
    assertthat::is.count(boot.R), boot.R > 1)
    #assertthat::is.count(ncpus)) #//doParallel
  # ==================================== #

  # set PRNG seed
  if (is.null(seed)) {
    seed <- .Random.seed #i.e., do not change anything
  } else {
    set.seed(seed)
  }


  # # Set up doParallel   #//DoParallel
  # local.cluster <- FALSE
  # if (ncpus > 1){
  #   cl.workers <- getDoParWorkers()
  #   if (cl.workers < ncpus){
  #     available.cores <- parallel::detectCores()
  #     if (ncpus >= available.cores){
  #       warning("ncpus too large, we only have ", available.cores, " cores. ",
  #               "Using ", available.cores - 1, " cores.")
  #       ncpus <- available.cores - 1
  #     }
  #     if (cl.workers < ncpus){
  #       cl.boot_sdm <- parallel::makeCluster(ncpus)
  #       doParallel::registerDoParallel(cl.boot_sdm)
  #       local.cluster <- TRUE
  #     }
  #   }
  # }

  # Perform bootstrap
  x.boot <- numeric(boot.R)
  for(i in 1:boot.R){
    x.boot[i] <- mean(sample(x, size = length(x), replace = TRUE))
  }

  #//DoParallel
  # x.boot <- foreach(i = 1:boot.R, .combine = c) %dopar%
  # {
  #   xbar <- mean(sample(x, size = length(x), replace = TRUE))
  # }

  # unregister cluster if needed          #//DoParallel
  # if (local.cluster) { stopCluster(cl.boot_sdm) }

  # Return standard error
  return(x.boot)
}
