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
#' @param boot.R number of bootstrap resamples
#' @param ncpus number of cores to use
#' @param seed seed for the PRNG
#'
#' @return vector of bootstrap estimates of the sample mean
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#' @export
#'
#' @examples
#' x <- rnorm(15, mean = 4, sd = 1)
#' my.sdm <- boot_sdm(x)
#' hist(my.sdm, breaks = 30)
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
                     ncpus  = 2,    # number of cores to use
                     seed   = NULL) # PRNG seed
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.numeric(x), length(x) > 1,
    assertthat::is.count(boot.R), boot.R > 1,
    assertthat::is.count(ncpus))
  # ==================================== #

  # set PRNG seed
  if (is.null(seed)) {
    seed <- .Random.seed #i.e., do not change anything
  } else {
    set.seed(seed)
  }

  # Perform bootstrap
  if(ncpus > 1){
    x.boot <- parallel::mclapply(1:boot.R,
                                 function(i){
                                   mean(sample(x,
                                               size = length(x),
                                               replace = TRUE))},
                                 mc.cores = ncpus)
  } else {
    x.boot <- lapply(1:boot.R,
                     function(i){
                       mean(sample(x,
                                   size = length(x),
                                   replace = TRUE))})
  }

  # Return standard error
  return(unlist(x.boot))
}
