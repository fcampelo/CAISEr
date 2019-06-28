#' Calculates number of instances for the comparison of multiple algorithms
#'
#' Calculates either the number of instances, the standardized effect sizes
#' detectable at a given power setting, or the power(s) of the comparisons of
#' multiple algorithms.
#'
#' The main use of this routine uses the closed formula of the t-test to
#' calculate the number of instances required for the comparison of pairs of
#' algorithms, given a desired power and standardized effect size of
#' interest. Significance levels of each comparison are adjusted using
#' Holm's step-down correction (the default). The routine also takes into
#' account whether the desired statistical power refers to the mean power
#' (the default), median, or worst-case (which is equivalent to
#' designing the experiment for the more widely-known Bonferroni correction).
#' See the reference by `Campelo and Wanner` for details.
#'
#' @section Sample Sizes for Nonparametric Methods:
#' If the parameter `test` is set to either `Wilcoxon` or `Binomial`, this
#' routine approximates the number of instances using the ARE of these tests
#' in relation to the paired t.test, using the formulas (see reference by
#' `Campelo and Takahashi` for details):
#'
#'   \deqn{n.wilcox = n.ttest / 0.86 = 1.163 * n.ttest}
#'   \deqn{n.binom = n.ttest / 0.637 = 1.570 * n.ttest}
#'
#' @param ncomparisons number of comparisons planned
#' @param ninstances the number of instances to be used in the experiment.
#' @param d minimally relevant effect size (MRES, expressed as a standardized
#'        effect size, i.e., "deviation from H0" / "standard deviation")
#' @param power target power for the comparisons (see `Details`)
#' @param sig.level desired family-wise significance level (alpha) for the
#'                  experiment
#' @param alternative type of alternative hypothesis to be performed
#'                    ("two.sided" or "one.sided")
#' @param test type of test to be used
#'                  ("t.test", "wilcoxon" or "binomial")
#' @param power.target which comparison should have the desired \code{power}?
#'                     Accepts "mean", "median", or "worst.case" (this last one
#'                     is equivalent to the Bonferroni correction).
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{ninstances} - number of instances
#'    \item \code{power} - the power of the comparison
#'    \item \code{d} - the effect size
#'    \item \code{sig.level} - significance level
#'    \item \code{alternative} - type of alternative hypothesis
#'    \item \code{test} - type of test
#' }
#'
#' @references
#' - P. Mathews.
#'    Sample size calculations: Practical methods for engineers and scientists.
#'    Mathews Malnar and Bailey, 2010.
#' - F. Campelo, F. Takahashi:
#'    Sample size estimation for power and accuracy in the experimental
#'    comparison of algorithms. Journal of Heuristics 25(2):305-338, 2019.
#' - F. Campelo, E. Wanner:
#'    Sample size calculations for the experimental comparison of multiple
#'    algorithms on multiple problem instances.
#'    Submitted, Journal of Heuristics, 2019.
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#'
#' @examples
#' # Calculate sample size for mean-case power
#' K      <- 10   # number of comparisons
#' alpha  <- 0.05 # significance level
#' power  <- 0.9  # desired power
#' d      <- 0.5  # MRES
#'
#' out <- calc_instances(K, d,
#'                       power     = power,
#'                       sig.level = alpha)
#'
#' # Plot power of each comparison to detect differences of magnitude d
#' plot(1:K, out$power,
#'      type = "b", pch = 20, las = 1, ylim = c(0, 1), xlab = "comparison",
#'      ylab = "power", xaxs = "i", xlim = c(0, 11))
#' grid(11, NA)
#' points(c(0, K+1), c(power, power), type = "l", col = 2, lty = 2, lwd = .5)
#' text(1, 0.93, sprintf("Mean power = %2.2f for N = %d",
#'                      out$mean.power, out$ninstances), adj = 0)
#'
#' # Check sample size if planning for Wilcoxon tests:
#' calc_instances(K, d,
#'                power     = power,
#'                sig.level = alpha,
#'                test = "wilcoxon")$ninstances
#'
#'
#' # Calculate power profile for predefined sample size
#' N <- 45
#' out2 <- calc_instances(K, d, ninstances = N, sig.level = alpha)
#'
#' points(1:K, out2$power, type = "b", pch = 19, col = 3)
#' text(6, .7, sprintf("Mean power = %2.2f for N = %d",
#'                      out2$mean.power, out2$ninstances), adj = 0)
#'
#' # Sample size for worst-case (Bonferroni) power of 0.8, using Wilcoxon
#' out3 <- calc_instances(K, d, power = 0.9, sig.level = alpha,
#'                        test = "wilcoxon", power.target = "worst.case")
#' out3$ninstances
#'
#' # For median power:
#' out4 <- calc_instances(K, d, power = 0.9, sig.level = alpha,
#'                        test = "wilcoxon", power.target = "median")
#' out4$ninstances
#' out4$power
#'
#' @export

# TESTED: OK
calc_instances <- function(ncomparisons,               # number of comparisons
                           d,                          # MRES
                           ninstances   = NULL,        # number of instances
                           power        = NULL,        # power
                           sig.level    = 0.05,        # significance level
                           alternative  = "two.sided", # type of H1
                           test         = "t.test",    # type of test
                           power.target = "mean")      # target power design
{

  test    <- match.arg(tolower(test),
                       c("t.test", "wilcoxon", "binomial"))
  alternative  <- match.arg(tolower(alternative),
                            c("one.sided", "two.sided"))
  power.target <- match.arg(tolower(power.target),
                            c("worst.case", "mean", "median"))


  # ========== Error catching ========== #
  assertthat::assert_that(
    assertthat::is.count(ncomparisons), ncomparisons > 1,
    is.null(ninstances) || (assertthat::is.count(ninstances) && ninstances > 1),
    is.null(power) || (is.numeric(power) && power > 0 && power < 1),
    is.null(d) || (is.numeric(d) && d > 0),
    sum(c(is.null(ninstances), is.null(power), is.null(d))) == 1,
    is.numeric(sig.level) && sig.level > 0 && sig.level < 1,
    alternative %in% c("one.sided", "two.sided"),
    test %in% c("t.test", "wilcoxon", "binomial"),
    power.target %in% c("worst.case", "mean", "median"))
  # ==================================== #

  # Calculate correction multiplier depending on test type
  # Based on the ARE of the tests (See Sheskin 1996)
  corr.factor <- switch(test,
                        t.test   = 1,
                        wilcoxon = 1 / 0.86,
                        binomial = 1 / 0.637)


  if (is.null(ninstances)){ # Estimate sample size
    if (power.target == "mean"){
      # Start by calculating N without any correction:
      N <- power.t.test(delta = d, sd = 1, sig.level = sig.level, power = power,
                        type = "paired", alternative = alternative)$n - 1

      p.mean <- 0 # mean power
      while (p.mean < power){
        N <- ceiling(N) + 1
        p <- numeric(ncomparisons)
        a <- numeric(ncomparisons)
        for (i in seq_along(p)){
          ss <- power.t.test(delta = d, sd = 1, n = N,
                             sig.level = sig.level / (ncomparisons - i + 1),
                             type = "paired",
                             alternative = alternative)
          p[i] <- ss$power
          a[i] <- ss$sig.level
        }
        p.mean <- mean(p)
      }
      p.median <- median(p)
      #
    } else {
      if (power.target == "worst.case") r <- 1 # Bonferroni correction
      if (power.target == "median")     r <- floor(ncomparisons / 2)

      N <- power.t.test(delta = d, sd = 1,
                        sig.level = sig.level / (ncomparisons - r + 1),
                        power = power,
                        type = "paired", alternative = alternative)$n

      # Calculate individual power of each comparison, + mean and median power
      p <- numeric(ncomparisons)
      a <- numeric(ncomparisons)
      for (i in seq_along(p)){
        ss <- power.t.test(delta = d, sd = 1, n = ceiling(N),
                           sig.level = sig.level / (ncomparisons - i + 1),
                           type = "paired",
                           alternative = alternative)
        p[i] <- ss$power
        a[i] <- ss$sig.level
      }
      p.mean   <- mean(p)
      p.median <- median(p)
    }

    # Adjust sample size depending on the type of test to be performed
    N <- ceiling(N * corr.factor)

  } else if (is.null(power)){ # calculate power profile of experiment
    N <- ninstances
    p <- numeric(ncomparisons)
    a <- numeric(ncomparisons)
    for (i in seq_along(p)){
      ss <- power.t.test(delta = d, sd = 1, n = ceiling(N / corr.factor),
                         sig.level = sig.level / (ncomparisons - i + 1),
                         type = "paired",
                         alternative = alternative)
      p[i] <- ss$power
      a[i] <- ss$sig.level
    }
    p.mean <- mean(p)
    p.median <- median(p)
  }


  output <- list(ninstances   = N,
                 power        = p,
                 mean.power   = p.mean,
                 median.power = p.median,
                 d            = d,
                 sig.level    = a,
                 alternative  = alternative,
                 test         = test,
                 power.target = power.target)

  return(output)
}
