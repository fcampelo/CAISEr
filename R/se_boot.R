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
#' @param ... other parameters (used only for compatibility with calls to
#'            [se_boot()], unused in this function)
#'
#' @return Data frame containing, for each pair of interest, the estimated
#'      difference (column "Phi") and the sample standard error (column "SE")
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#' @export
#'
#' @examples
#' # three vectors of normally distributed observations
#' set.seed(1234)
#' Xk <- list(rnorm(10, 5, 1),  # mean = 5, sd = 1,
#'            rnorm(20, 10, 2), # mean = 10, sd = 2,
#'            rnorm(20, 15, 5)) # mean = 15, sd = 3
#'
#' se_boot(Xk, dif = "simple", comparisons = "all.vs.all")
#' se_boot(Xk, dif = "perc", comparisons = "all.vs.first")
#' se_boot(Xk, dif = "perc", comparisons = "all.vs.all")

# TESTED: OK
se_boot <- function(Xk,                  # vector of observations
                    dif = "simple",      # type of difference
                    comparisons = "all.vs.all", # standard errors to calculate
                    boot.R = 999)        # number of bootstrap resamples
{

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.list(Xk),
    all(sapply(Xk, is.numeric)),
    all(sapply(Xk, function(x){length(x) >= 2})),
    dif %in% c('simple', 'perc'),
    comparisons %in% c("all.vs.all", "all.vs.first"),
    assertthat::is.count(boot.R), boot.R > 1)
  # ==================================== #

  nalgs <- length(Xk)
  Nk    <- sapply(Xk, length)

  # Get pairs for calculation
  algo.pairs <- t(utils::combn(1:length(Xk), 2))
  if (comparisons == "all.vs.first") algo.pairs <- algo.pairs[1:(nalgs - 1), , drop = FALSE]

  # Calculate point estimates and standard errors for all required pairs
  Phik  <- numeric(nrow(algo.pairs))
  SEk   <- numeric(nrow(algo.pairs))
  Roptk <- numeric(nrow(algo.pairs))


  for (k in 1:nrow(algo.pairs)){
    ind      <- as.numeric(algo.pairs[k, ])
    phi.hat  <- numeric(boot.R)
    ropt.hat <- numeric(boot.R)

    for(i in 1:boot.R){
      # Resample everyone with replacement
      Xk.b <- mapply(FUN = sample,
                     Xk, lapply(Xk, length),
                     MoreArgs = list(replace = TRUE),
                     SIMPLIFY = FALSE)

      # Calculate relevant statistics for this bootstrap replicate
      Vark     <- sapply(Xk.b, stats::var)
      Xbark    <- sapply(Xk.b, mean)
      Xbar.all <- mean(Xbark)

      if (dif == "simple") {
        # mu1 - mu2
        phi.hat[i] <- Xbark[ind[1]] - Xbark[ind[2]]
        # s1 / s2
        ropt.hat[i] <- sqrt(Vark[ind[1]] / Vark[ind[2]])
        #
      } else if (dif == "perc"){
        if (comparisons == "all.vs.all"){
          # (mu1 - mu2) / mu
          phi.hat[i] <- (Xbark[ind[1]] - Xbark[ind[2]]) / Xbar.all
          # r = s1 / s2
          ropt.hat[i] <- sqrt(Vark[ind[1]] / Vark[ind[2]])
          #
        } else if (comparisons == "all.vs.first"){
          # (mu1 - mu2) / mu1
          phi.hat[i] <- 1 - Xbark[ind[2]] / Xbark[ind[1]]
          # r = (s1 / s2) * (mu2 / mu1)
          ropt.hat[i] <- sqrt(Vark[ind[1]] / Vark[ind[2]]) * (Xbark[ind[2]] / Xbark[ind[1]])
          #
        } else stop("comparisons option *", comparisons, "* not recognized.")
        #
      } else stop ("dif option *", dif, "* not recognized.")
    }
    # Estimate quantities of interest
    Phik[k]  <- mean(phi.hat)
    SEk[k]   <- stats::sd(phi.hat)
    Roptk[k] <- mean(ropt.hat)
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
  return(output)

}
