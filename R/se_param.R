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
#'    Applied Statistics and Probability for Engineers, 6th edn. Wiley (2013)
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms (submitted, 2017).
#'
#' @param x1 vector of observations
#' @param x2 vector of observations
#' @param dif name of the difference for which the SE is desired. Accepts
#'            "simple" (simple differences) or "perc" (percent differences).
#'
#' @return estimated standard error
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'

se_param <- function(x1,  # vector of observations
                     x2,  # vector of observations
                     dif) # type of statistic
{

    # ========== Error catching ========== #
    assertthat::assert_that(
        is.numeric(x1), is.vector(x1), length(x1) > 1,
        is.numeric(x2), is.vector(x2), length(x2) > 1,
        dif %in% c('simple', 'perc'))
    # ==================================== #

    # Estimators
    v1    <- var(x1)
    v2    <- var(x2)
    xbar1 <- mean(x1)
    xbar2 <- mean(x2)
    n1    <- length(x1)
    n2    <- length(x2)

    if (dif == "simple") {
        se <- sqrt(v1 / n1 + v2 / n2)
    } else if (dif == "perc") {
        phi1 <- xbar2 - xbar1
        c1 <- v1 * (1 / (phi1 ^ 2) + 1 / (xbar1 ^ 2))
        c2 <- v2 / (phi1 ^ 2)
        se <- abs(phi1) * sqrt(c1 / n1 + c2 / n2)
    } else stop ("dif option", dif, "not recognized.")

    # Return standard error
    return(se)
}
