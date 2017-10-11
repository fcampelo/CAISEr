#' Percent differences estimator
#'
#' Calculates the sample estimator of \eqn{(mu2 - mu1) / mu1}, where 
#' \eqn{mu1, mu2} are the means of the populations that generated the sample 
#' vectors \eqn{x1, x2}.
#'
#' @section References:
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms (submitted, 2017).
#'
#' @param x1 vector of observations
#' @param x2 vector of observations
#'
#' @return Estimated value of the statistic
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'

est_perc <- function(x1, x2){

    # ========== Error catching ========== #
    assertthat::assert_that(is.numeric(x1),
                            is.numeric(x2),
                            length(x1) > 1,
                            length(x2) > 1)
    # ==================================== #

    # Calculate phi_j and return
    phi_j <- (mean(x2) - mean(x1)) / mean(x1)
    return(phi_j)
}
