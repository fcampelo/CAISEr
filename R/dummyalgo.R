#' Dummy algorithm routine to test the sampling procedures
#'
#' This is a dummy algorithm routine to test the sampling procedures, in
#' combination with [dummyinstance()].
#' `dummyalgo()` receives two parameters that determine the distribution of
#'  performances it will exhibit on a hypothetical problem class:
#' `distribution.fun` (with the name of a random number generation function,
#' e.g. `rnorm`, `runif`, `rexp` etc.); and `distribution.pars`, a named list of
#' parameters to be passed on to `distribution.fun`.
#' The third parameter is an instance object (see [calc_nreps()] for details),
#' which is a named list with the following fields:
#'   \itemize{
#'      \item\code{FUN = "dummyinstance"} - must always be "dummyinstance" (will
#'      be ignored otherwise).
#'      \item\code{distr} - the name of a random number generation function.
#'      \item\code{...} - other named fields with parameters to be passed down
#'                        to the function in `distr`.
#'   }
#'
#' `distribution.fun` and `distribution.pars` regulate the mean performance of
#' the dummy algorithm on a given (hypothetical) problem class, and the
#' between-instances variance of performance. The instance specification in
#' `instance` regulates the within-instance variability of results. Ideally the
#' distribution parameters passed to the `instance` should result in a
#' within-instance distribution of values with zero mean, so that the mean of
#' the values returned by `dummyalgo` is regulated only by `distribution.fun`
#' and `distribution.pars`.
#'
#' The value returned by dummyalgo is sampled as follows:
#' \preformatted{
#'    offset <- do.call(distribution.fun, args = distribution.pars)
#'    y <- offset + do.call("dummyinstance", args = instance)
#' }
#'
#' @param distribution.fun name of a function that generates random values
#' according to a given distribution, e.g., "rnorm", "runif", "rexp" etc.
#' @param distribution.pars list of named parameters required by the function
#' in \code{distribution.fun}. Parameter \code{n} (number of points to
#' generate) is unnecessary (this routine always forces `n = 1`).
#' @param instance instance parameters (see `Details`).
#'
#' @return a list object with a single field \code{$value}, containing a scalar
#' numerical value distributed as described at the end of `Details`.
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#' @seealso [dummyinstance()]
#'
#' @export
#'
#' @examples
#' # Make a dummy instance with a centered (zero-mean) exponential distribution:
#' instance = list(FUN = "dummyinstance", distr = "rexp", rate = 5, bias = -1/5)
#'
#' # Simulate a dummy algorithm that has a uniform distribution of expected
#' # performance values, between -25 and 50.
#' dummyalgo(distribution.fun = "runif",
#'           distribution.pars = list(min = -25, max = 50),
#'           instance = instance)
#'

# TESTED: OK
dummyalgo <- function(distribution.fun = "rnorm",
                      distribution.pars = list(mean = 0, sd = 1),
                      instance = list(FUN = "dummyinstance",
                                      distr = "rnorm",
                                      mean = 0,
                                      sd   = 1))
{
  # ========== Error catching ========== #
  assertthat::assert_that(
    is.character(distribution.fun),
    length(distribution.fun) == 1,
    is.list(distribution.pars),
    is.list(instance),
    all(c("FUN", "distr") %in% names(instance)))
  # ==================================== #

  distribution.pars$n <- 1
  offset <- do.call(distribution.fun, args = distribution.pars)
  y      <- offset + do.call("dummyinstance", args = instance)
  return(list(value = y))
}
