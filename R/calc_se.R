#' Calculates the standard error for simple and percent differences
#'
#' Calculates the sample standard error for the estimator differences between
#' multiple algorithms on a given instance.
#'
#' - If `dif == "perc"` it returns the standard errors for the sample
#' estimates of pairs
#'   \eqn{(mu2 - mu1) / mu}, where \eqn{mu1, mu2} are the means of the
#'   populations that generated sample vectors \eqn{x1, x2}, and
#' - If `dif == "simple"` it returns the SE for sample estimator of
#'   \eqn{(mu2 - mu1)}
#'
#' @section References:
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms. Journal of Heuristics 25(2):305-338, 2019.
#'
#' @param Xk list object where each position contains a vector of observations
#'           of algorithm k on a given problem instance.
#' @param dif name of the difference for which the SEs are desired.
#'            Accepts "perc" (for percent differences) or "simple" (for simple
#'            differences)
#' @param type standard errors to be calculated. Accepts "all.vs.first"
#'          (in which cases the first object in `algorithms` is considered to be
#'          the reference algorithm) or "all.vs.all" (if there is no reference
#'          and all pairwise SEs are desired).
#' @param method method used to calculate the interval. Accepts "param"
#'          (using parametric formulas based on normality of the sampling
#'          distribution of the means) or "boot" (for bootstrap).
#' @param boot.R (optional) number of bootstrap resamples
#'               (if `method == "boot"`)
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{Phi.est} - estimated values of the statistic of interest for
#'    each pair of algorithms of interest (all pairs if `type == "all.vs.all"`,
#'    or all pairs containing the first algorithm if `type == "all.vs.first"`).
#'    \item \code{se} - standard error estimates
#' }
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
#' calc_se(Xk, dif = "simple", type = "all.vs.all", method = "param")
#'
#' calc_se(Xk, dif = "perc", type = "all.vs.first", method = "param")


calc_se <- function(Xk,                  # vector of observations
                    dif = "simple",      # type of difference
                    type = "all.vs.all", # standard errors to calculate
                    method = "param",    # method for calculating CI
                    boot.R = 999)        # number of bootstrap resamples

{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.list(Xk),
    all(sapply(Xk, is.numeric)),
    all(sapply(Xk, function(x){length(x) >= 2})),
    dif %in% c('simple', 'perc'),
    type %in% c("all.vs.all", "all.vs.first"),
    method %in% c('param', 'boot'),
    assertthat::is.count(boot.R))
  # ==================================== #

  # Calculate point estimates and standard errors
  if (method == "param"){
    Diffk <- se_param(Xk = Xk, dif = dif, type = type)
  } else if (method == "boot"){
    Diffk <- se_boot(Xk = Xk, dif = dif, type = type,
                  boot.R = boot.R)
  }

  # Fix NaN problem that happens if some variance = 0
  Diffk$SE[is.nan(Diffk$SE)] <- 0

  return(Diffk)
}
