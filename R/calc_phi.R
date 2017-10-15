#' Calculates the sample estimator of (simple or percent) differences
#'
#' Calculates the sample estimator of the (simple or percent) differences
#' between the means of two algorithms on a given instance.
#' - If `dif == "simple": estimates \eqn{mu2 - mu1},
#' - If `dif == "perc": estimates \eqn{(mu2 - mu1) / mu1},
#'
#' where \eqn{mu1, mu2} are the means of the populations that generated the
#' sample vectors \eqn{x1, x2}.
#'
#' @section References:
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms (submitted, 2017).
#'
#' @param x1 vector of observations
#' @param x2 vector of observations
#' @param dif type of difference to estimate ("simple" or "perc")
#'
#' @return Estimated value of the statistic given in `dif`
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @examples
#' x1 <- rnorm(25, 3, 0.5)
#' x2 <- runif(15, 4, 6)
#' calc_phi(x1, x2, "simple")
#' calc_phi(x1, x2, "perc")

# TESTED
calc_phi <- function(x1, x2, dif){

  # ========== Error catching ========== #
  assertthat::assert_that(is.numeric(x1),
                          is.numeric(x2),
                          length(x1) > 1,
                          length(x2) > 1,
                          dif %in% c("simple", "perc"))
  # ==================================== #

  # Calculate phi_j and return
  if (dif == "simple"){
    phi_j <- mean(x2) - mean(x1)
  } else if (dif == "perc"){
    phi_j <- (mean(x2) - mean(x1)) / mean(x1)
  } else stop ("dif option", dif, "not recognized.")

  return(phi_j)
}
