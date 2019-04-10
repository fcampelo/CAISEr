#' Parametric standard errors
#'
#' Calculates the standard error of a given statistic using parametric formulas
#'
#' @section References:
#' -  E.C. Fieller:
#'     Some problems in interval estimation. Journal of the Royal Statistical
#'     Society. Series B (Methodological) 16(2), 175–185 (1954)
#' - V. Franz:
#'    Ratios: A short guide to confidence limits and proper use (2007).
#'    https://arxiv.org/pdf/0710.2024v1.pdf
#' - D.C. Montgomery, C.G. Runger:
#'    Applied Statistics and Probability for Engineers, 6th ed. Wiley (2013)
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms. Journal of Heuristics 25(2):305-338, 2019.
#'
#' @inheritParams calc_sek
#' @param ... other parameters (used only for compatibility with calls to
#'            [se_boot()], unused in this function)
#'
#' @return estimated standard errors
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @export
#'
#' @examples
#' # three vectors of normally distributed observations
#' set.seed(1234)
#' Xk <- list(rnorm(100, 5, 1)  # mean = 5, sd = 1,
#'            rnorm(200, 10, 2) # mean = 10, sd = 2,
#'            rnorm(500, 15, 3) # mean = 15, sd = 3)
#'
#' se_param(Xk, dif = "simple", type = "all.vs.all")
#' se_param(Xk, dif = "percent", type = "all.vs.first")

# UNTESTED
se_param <- function(x1,  # vector of observations
                     x2,  # vector of observations
                     dif, # type of statistic
                     ...)
{

    # ========== Error catching ========== #
    assertthat::assert_that(
        is.numeric(x1), is.vector(x1), length(x1) > 1,
        is.numeric(x2), is.vector(x2), length(x2) > 1,
        dif %in% c('simple', 'perc'))
    # ==================================== #

    # Estimates
    v1    <- stats::var(x1)
    v2    <- stats::var(x2)
    xbar1 <- mean(x1)
    xbar2 <- mean(x2)
    n1    <- length(x1)
    n2    <- length(x2)

    # Calculate SE
    if (dif == "simple") {
        se <- sqrt(v1 / n1 + v2 / n2)
    } else if (dif == "perc") {
        phi1 <- xbar2 - xbar1
        phi2 <- (xbar2 - xbar1) / xbar1
        c1 <- v1 * (1 / (phi1 ^ 2) + 1 / (xbar1 ^ 2))
        c2 <- v2 / (phi1 ^ 2)
        se <- abs(phi2) * sqrt(c1 / n1 + c2 / n2)
    } else stop ("dif option", dif, "not recognized.")

    # Return standard error
    return(se)
}
