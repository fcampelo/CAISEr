#' Parametric standard errors
#'
#' Calculates the standard errors of a given statistic using parametric formulas
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
#' @inheritParams calc_se
#' @param ... other parameters (used only for compatibility with calls to
#'            [se_boot()], unused in this function)
#'
#' @return Data frame containing, for each pair of interest, the estimated
#'      difference (column "Phi") and the sample standard error (column "SE")
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
#' se_param(Xk, dif = "simple", type = "all.vs.all")
#' se_param(Xk, dif = "perc", type = "all.vs.first")

# TESTED
se_param <- function(Xk,                  # vector of observations
                     dif = "simple",      # type of difference
                     type = "all.vs.all", # standard errors to calculate
                     ...)
{

    # ========== Error catching ========== #
    assertthat::assert_that(
      is.list(Xk),
      all(sapply(Xk, is.numeric)),
      all(sapply(Xk, function(x){length(x) >= 2})),
      dif %in% c('simple', 'perc'),
      type %in% c("all.vs.all", "all.vs.first"))
    # ==================================== #

    # Estimates
    nalgs    <- length(Xk)
    Vark     <- sapply(Xk, stats::var)
    Xbark    <- sapply(Xk, mean)
    Nk       <- sapply(Xk, length)
    Xbar.all <- mean(Xbark)

    # Get pairs for comparison
    algo.pairs <- t(combn(1:length(Xk), 2))
    if (type == "all.vs.first") algo.pairs <- algo.pairs[1:(nalgs - 1), ]

    # Calculate point estimates and standard errors for all required pairs
    Phik  <- numeric(nrow(algo.pairs))
    SEk   <- numeric(nrow(algo.pairs))
    Roptk <- numeric(nrow(algo.pairs))
    for (i in 1:nrow(algo.pairs)){
      ind <- as.numeric(algo.pairs[i, ])
      if (dif == "simple") {
        Phik[i]  <- Xbark[ind[1]] - Xbark[ind[2]]
        SEk[i]   <- sqrt(sum(Vark[ind] / Nk[ind]))
        Roptk[i] <- sqrt(Vark[ind[1]] / Vark[ind[2]])
      } else if (dif == "perc"){
        if (type == "all.vs.all"){
          Phik[i] <- (Xbark[ind[1]] - Xbark[ind[2]]) / Xbar.all
          C1 <- (1 + (Phik[i] / nalgs) ^ 2) / (Xbar.all ^ 2)
          C2 <- sum(Vark[-ind] / Nk[-ind]) * ((Phik[i] / nalgs) ^ 2) / (Xbar.all ^ 2)
          SEk[i]  <- C1 * (sum(Vark[ind] / Nk[ind])) + C2
          Roptk[i] <- sqrt(Vark[ind[1]] / Vark[ind[2]])
        } else if (type == "all.vs.first"){
          Phik[i] <- 1 - Xbark[ind[2]] / Xbark[ind[1]]
          C1      <- Vark[ind[1]] * (Xbark[ind[2]] / (Xbark[ind[1]] ^ 2)) ^2

          # PAREI

          C2      <- (1 + Phik[i] ^ 2) * Vark[ind[2]] / (Xbark[ind[2]] ^ 2)
          SEk[i]  <- C1 / Nk[ind[1]] + C2 / Nk[ind[2]]
          Roptk[i] <- sqrt((Vark[ind[1]] * Xbark[ind[2]] ^ 2) /
                           (Vark[ind[2]] * (Xbark[ind[2]] ^ 2 +
                                            (Xbark[ind[1]] - Xbark[ind[2]]) ^ 2)))
        } else stop("type option *", type, "* not recognized.")
      } else stop ("dif option *", dif, "* not recognized.")
    }

    # Assemble data frame with results
    output <- data.frame(Alg1 = algo.pairs[, 1],
                         Alg2 = algo.pairs[, 2],
                         N1   = Nk[algo.pairs[, 1]],
                         N2   = Nk[algo.pairs[, 2]],
                         Phi  = Phik,
                         SE   = SEk,
                         r    = Nk[algo.pairs[, 1]] / Nk[algo.pairs[, 2]],
                         ropt = Roptk)
    return(output)
}
