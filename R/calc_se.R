#' Calculates the standard error for simple and percent differences
#'
#' Calculates the sample standard error for the estimator differences between
#' two algorithms on a given instance.
#'
#' - If `dif == "perc"` it returns the SE for sample estimator of
#'   \eqn{(mu2 - mu1) / mu1}, where \eqn{mu1, mu2} are the means of the
#'   populations that generated the sample vectors \eqn{x1, x2}.
#' - If `dif == "simple"` it returns the SE for sample estimator of
#'   \eqn{(mu2 - mu1)}
#'
#' @section References:
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms (submitted, 2017).
#'
#' @param x1 vector of observations
#' @param x2 vector of observations
#' @param dif name of the difference for which the SE is desired. Accepts "perc"
#'          (for percent differences) or "simple" (for simple differences)
#' @param method method used to calculate the interval. Accepts "param"
#'          (using parametric formulas based on normality of the sampling
#'          distribution of the means) or "boot" (for bootstrap).
#' @param boot.R (optional) number of bootstrap resamples.
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{x.est} - estimated value
#'    \item \code{se} - standard error
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Fernanda Takahashi (\email{fernandact@@ufmg.br})
#'
#' @examples
#' # two vectors of normally distributed observations
#' set.seed(1234)
#' x1 <- rnorm(100, 5, 1)  # mean = 5, sd = 1
#' x2 <- rnorm(200, 10, 2) # mean = 10, sd = 2
#'
#' # Theoretical SE for simple difference: 0.1732051
#' calc_se(x1, x2, dif = "simple", method = "param")
#'
#' # Theoretical (Fieller, no covariance) SE for percent differences: 0.04
#' calc_se(x1, x2, dif = "perc", method = "boot")

# TESTED
calc_se <- function(x1,           # vector of observations
                    x2,           # vector of observations
                    dif,          # type of statistic
                    method = "param",   # method for calculating CI
                    boot.R = 999) # number of bootstrap resamples

{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.numeric(x1), is.vector(x1), length(x1) > 1,
    is.numeric(x2), is.vector(x2), length(x2) > 1,
    dif %in% c('simple', 'perc'),
    method %in% c('param', 'boot'),
    assertthat::is.count(boot.R), boot.R > 1)
  # ==================================== #

  if (method == "param"){
    se <- se_param(x1 = x1, x2 = x2, dif = dif)
  } else if (method == "boot"){
    se <- se_boot(x1 = x1, x2 = x2, dif = dif,
                  boot.R = boot.R)
  }

  x.est <- do.call(calc_phi,
                   args = list(x1 = x1, x2 = x2, dif = dif))

  return(list(x.est = x.est,
              se    = se))
}
