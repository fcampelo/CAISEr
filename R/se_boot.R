#' Bootstrap standard errors
#'
#' Calculates the standard errors of a given statistic using bootstrap
#'
#' @section References:
#' -  A.C. Davison, D.V. Hinkley:
#'    Bootstrap methods and their application. Cambridge University Press (1997)
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms. Journal of Heuristics 25(2):305-338, 2019.
#'
#' @inheritParams calc_se
#'
#' @return estimated standard error
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @export
#'
#' @examples
#' # three vectors of normally distributed observations
#' set.seed(1234)
#' Xk <- list(rnorm(100, 5, 1),  # mean = 5, sd = 1,
#'            rnorm(200, 10, 2), # mean = 10, sd = 2,
#'            rnorm(500, 15, 3)) # mean = 15, sd = 3
#'
#' se_boot(Xk, dif = "simple", type = "all.vs.all")
#' se_boot(Xk, dif = "perc", type = "all.vs.first")

# TO DO
se_boot <- function(Xk,                  # vector of observations
                    dif = "simple",      # type of difference
                    type = "all.vs.all", # standard errors to calculate
                    boot.R = 999)        # number of bootstrap resamples
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.list(Xk),
    all(sapply(Xk, is.numeric)),
    all(sapply(Xk, function(x){length(x) >= 2})),
    dif %in% c('simple', 'perc'),
    type %in% c("all.vs.all", "all.vs.first"),
    assertthat::is.count(boot.R), boot.R > 1)
  # ==================================== #

  # Get pairs for comparison
  algo.pairs <- t(combn(1:length(Xk), 2))
  if (type == "all.vs.first") algo.pairs <- algo.pairs[1:(nalgs - 1), ]

  stop("se_boot still under construction")
  # Phi.hat <- vector("list", length = nrow(algo.pairs))
  # # Perform bootstrap
  #
  # for(i in 1:boot.R)
  # {
  #   x1.b <- sample(x1, size = length(x1), replace = TRUE)
  #   x2.b <- sample(x2, size = length(x2), replace = TRUE)
  #   phi.hat[i] <- calc_phi(x1 = x1.b, x2 = x2.b, dif = dif)
  # }
  #
  # # Return standard error
  # return(stats::sd(phi.hat))
}
