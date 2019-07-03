#' Dummy instance routine to test the sampling procedures
#'
#' This is a dummy instance routine to test the sampling procedures, in
#' combination with [dummyalgo()].
#' `dummyinstance()` receives a parameter `distr` containing the name of a
#' random number generation function (e.g. `rnorm`, `runif`, `rexp` etc.), plus
#' a variable number of arguments to be passed down to the function in `distr`.
#'
#' @param distr name of a function that generates random values
#' according to a given distribution, e.g., "rnorm", "runif", "rexp" etc.
#' @param ... additional parameters to be passed down to the function in
#' `distr`. Parameter \code{n} (number of points to generate) is unnecessary
#' (this routine always forces `n = 1`).
#' @param bias a bias term to add to the results of the distribution function
#' (e.g., to set the mean to zero).
#'
#' @return a single numeric value sampled from the desired distribution.
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#' @seealso [dummyalgo()]
#'
#' @export
#'
#' @examples
#' dummyinstance(distr = "rnorm", mean = 10, sd = 1)
#'
#' # Make a centered (zero-mean) exponential distribution:
#' lambda = 4
#'
#' # 10000 observations
#' set.seed(1234)
#' y <- numeric(10000)
#' for (i in 1:10000) y[i] <- dummyinstance(distr = "rexp", rate = lambda,
#'                                          bias = -1/lambda)
#' mean(y)
#' hist(y, breaks = 50, xlim = c(-0.5, 2.5))

# TESTED: OK
dummyinstance <- function(distr, ..., bias = 0){
  # ========== Error catching ========== #
  assertthat::assert_that(
    is.character(distr),
    length(distr) == 1,
    is.numeric(bias), length(bias) == 1)
  # ==================================== #

  args   <- list(...)
  if ("FUN" %in% names(args))   args$FUN   <- NULL
  if ("alias" %in% names(args)) args$alias <- NULL

  args$n <- 1
  return(bias + do.call(distr, args = args))
}
