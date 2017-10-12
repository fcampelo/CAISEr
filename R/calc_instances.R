#' Calcultes number of instances for the comparison of two algorithms
#'
#' Calculates either the number of instances, the standardized effect size, or
#' the power of a comparison of two algorithms
#'
#' This routine uses the closed formula of the t-test to calculate the number
#' of instances required for a comparison of two algorithms, considering a
#' desired power, standardized effect size, and significance level.
#' for cases where the number of instaces is predefined, it can return the
#' test power instead.
#'
#' @section Sample Sizes for Nonparametric Methods:
#' If the parameter `test.type` is set to either `Wilcoxon` or `Binomial`, this
#' routine approximates the number of instances using the ARE of these tests
#' in relation to the paired t.test, using the formulas:
#'   - \deqn{n.wilcox = n.ttest / 0.86 = 1.163 * n.ttest}
#'   - \deqn{n.binom = n.ttest / 0.637 = 1.570 * n.ttest}
#'
#' @param ninstances the number of instances to be used in the experiment.
#' @param power (desired) test power
#' @param d minimally relevant effect size (MRES, expressed as a standardized
#'        effect size, i.e., "deviation from H0" / "standard deviation")
#' @param sig.level significance level (alpha) for the experiment
#' @param alternative type of alternative hypothesis ("two.sided" or
#'                    "one.sided")
#' @param test.type type of test ("t.test", "wilcoxon", "binomial")
#'
#' @return a list object containing the following items:
#' \itemize{
#'    \item \code{ninstances} - number of instances
#'    \item \code{power} - the power of the comparision
#'    \item \code{d} - the effect size
#'    \item \code{sig.level} - significance level
#'    \item \code{alternative} - type of alternative hypothesis
#'    \item \code{test.type} - type of test
#' }
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Fernanda Takahashi (\email{fernandact@@ufmg.br})
#'
#' @export

calc_instances <- function(ninstances  = NULL,        # number of instances
                           power       = NULL,        # power
                           d,                         # MRES
                           sig.level   = 0.05,        # significance level
                           alternative = "two.sided", # type of H1
                           test.type   = "t.test")    # type of test
{

  test.type   <- match.arg(test.type, c("t.test", "wilcoxon", "binomial"))
  alternative <- match.arg(alternative, c("one.sided", "two.sided"))

  # ========== Error catching ========== #
  assertthat::assert_that(
    is.null(ninstances) || (assertthat::is.count(ninstances) && ninstances > 1),
    is.null(power) || (is.numeric(power) && power > 0 && power < 1),
    sum(c(is.null(ninstances), is.null(power))) == 1,
    is.numeric(sig.level) && sig.level > 0 && sig.level < 1,
    is.numeric(d),
    alternative %in% c("one.sided", "two.sided"),
    test.type %in% c("t.test", "wilcoxon", "binomial"))
  # ==================================== #

  # Get correction factor depending on the type of test
  corr.factor <- switch(test.type,
                        t.test   = 1,
                        wilcoxon = 1 / 0.86,
                        binomial = 1 / 0.637)


  if (is.null(ninstances)){ # Estimate sample size
    ss <- power.t.test(n           = ninstances,
                       delta       = d, sd = 1,
                       sig.level   = sig.level,
                       power       = power,
                       type        = "paired",
                       alternative = alternative,
                       strict      = TRUE)
    ss$n <- ss$n * corr.factor

  } else if (is.null(power)){ # Estimate power
    ss <- power.t.test(n           = ninstances / corr.factor,
                       delta       = d, sd = 1,
                       sig.level   = sig.level,
                       power       = power,
                       type        = "paired",
                       alternative = alternative,
                       strict      = TRUE)
    ss$n <- ninstances

  }

  output     <- list(ninstances  = ceiling(ss$n),
                     power       = ss$power,
                     d           = ss$delta / ss$sd,
                     sig.level   = ss$sig.level,
                     alternative = alternative,
                     test.type   = test.type)

  return(output)
}
