#' Calculates the optimal ratio of sample sizes
#'
#' Calculates the optimal ratio of sample sizes of two algorithms on a given
#' instance, for either simple or percent differences, using the parametric
#' approach.
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
#'
#' @return numeric value: optimal ratio n1 / n2
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @examples
#' set.seed(1234)
#' x1 <- rnorm(25, 5, 1)
#' x2 <- runif(35, 8, 10)
#' calc_ropt(x1, x2, "simple")
#' calc_ropt(x1, x2, "perc")

# TESTED
calc_ropt <- function(x1,   # vector of observations
                      x2,   # vector of observations
                      dif)  # type of statistic

{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.numeric(x1), is.vector(x1), length(x1) > 1,
    is.numeric(x2), is.vector(x2), length(x2) > 1,
    dif %in% c('simple', 'perc'))
  # ==================================== #

  s1 <- sd(x1)
  s2 <- sd(x2)
  if (dif == "simple"){
    r.opt <- s1 / s2
  } else if (dif == "perc"){
    phi   <- calc_phi(x1 = x1, x2 = x2, dif = "perc")
    r.opt <- (s1 / s2) * sqrt(1 + phi ^ 2)
  } else stop ("dif option", dif, "not recognized.")

  return(r.opt)
}
