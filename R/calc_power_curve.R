#' Calculate the power curve for an experiment
#'
#' Calculates the power curve (d x power) for an experiment with a fixed
#' number of instances. See [calc_instances()] for details.
#'
#'
#' @inheritParams calc_instances
#' @param d.range vector `c(dmin, dmax)` with range of effect sizes to consider
#'        for the power calculations.
#' @param npoints number of points for the power curve.
#'
#' @return an object of class `caiser.powercurve` containing fields
#' `d`, the (standardized) effect size; and `power`, the (expected) power
#' to detect each effect size in `d`.
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#' @examples
#' my.cpc <- calc_power_curve(ninstances = 10)
#' summary(my.cpc)
#' plot(my.cpc)
#'
#'
#' @export

calc_power_curve <- function(ninstances,         # number of instances
                             sig.level   = 0.05, # significance level
                             alternative = "two.sided", # type of H1
                             test.type   = "t.test",
                             d.range     = c(0.1, 2),
                             npoints     = 100)    # type of test
{

  test.type   <- match.arg(test.type, c("t.test", "wilcoxon", "binomial"))
  alternative <- match.arg(alternative, c("one.sided", "two.sided"))

  # ========== Error catching ========== #
  assertthat::assert_that(assertthat::is.count(npoints),
                          npoints > 1,
                          is.numeric(d.range),
                          length(d.range) == 2,
                          d.range[1] < d.range[2])
  # ==================================== #

  D <- seq(from = d.range[1], to = d.range[2], length.out = npoints)

  mypowers <- unlist(lapply(D,
                            FUN = function(x){
                              calc_instances(ninstances  = ninstances,
                                             power = NULL,
                                             d = x,
                                             sig.level   = sig.level,
                                             alternative = alternative,
                                             test.type   = test.type)$power}))


  output <- list(d     = D,
                 power = mypowers)
  class(output) <- c("CAISErPowercurve", "list")
  return(output)
}
