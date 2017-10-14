#' Bootstrap standard errors
#'
#' Calculates the standard error of a given statistic using bootstrap
#'
#' @section References:
#' -  A.C. Davison, D.V. Hinkley:
#'    Bootstrap methods and their application. Cambridge University Press (1997)
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms (submitted, 2017).
#'
#' @param x1 vector of observations
#' @param x2 vector of observations
#' @param dif name of the difference for which the SE is desired. Accepts
#'            "simple" (simple differences) or "perc" (percent differences).
#' @param boot.R (optional) number of bootstrap resamples
#'
#' @return estimated standard error
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @examples
#' # two vectors of normally distributed observations
#' set.seed(1234)
#' x1 <- rnorm(100, 5, 1)  # mean = 5, sd = 1
#' x2 <- rnorm(200, 10, 2) # mean = 10, sd = 2
#'
#' # Theoretical SE for simple difference: 0.1732051
#' se_boot(x1, x2, dif = "simple")
#'
#' # Theoretical (Fieller, no covariance) SE for percent differences: 0.04
#' se_boot(x1, x2, dif = "perc")

# TESTED
se_boot <- function(x1,           # vector of observations
                    x2,           # vector of observations
                    dif,          # type of statistic
                    boot.R = 999) # number of bootstrap resamples
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.numeric(x1), is.vector(x1), length(x1) > 1,
    is.numeric(x2), is.vector(x2), length(x2) > 1,
    assertthat::is.count(boot.R), boot.R > 1,
    dif %in% c('simple', 'perc'))
  # ==================================== #

  # Estimators
  if (dif == "simple") {
    phi <- function(x1, x2){ mean(x2) - mean(x1) }
  } else if (dif == "perc") {
    phi <- function(x1, x2){ (mean(x2) - mean(x1)) / mean(x1) }
  } else stop ("dif option", dif, "not recognized.")

  # Perform bootstrap
  phi.hat <- foreach(i = 1:boot.R, .combine = c) %dopar%
  {
    x1.b <- sample(x1, size = length(x1), replace = TRUE)
    x2.b <- sample(x2, size = length(x2), replace = TRUE)
    phi(x1 = x1.b, x2 = x2.b)
  }

  # Return standard error
  return(sd(phi.hat))
}
