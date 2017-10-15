#' Dummy algorithm routine to test the sampling procedures
#'
#' This is a dummy algorithm routine to test the sampling procedures. It
#' basically returns values according to the distribution function given
#' in \code{distribution.fun} with parameters given by \code{distribution.pars}.
#'
#' @param distribution.fun name of a function that generates random values
#' according to a given distribution, e.g., "rnorm", "runif", "rexp" etc.
#' @param distribution.pars list of named parameters required by the function
#' in \code{distribution.fun}. Parameter \code{n} (number of points to
#' generate) is unnecessary (this routine always considers `n = 1`).
#' @param instance instance parameters. This parameter is always ignored, and
#' was only included for compatibility with \code{run_nreps2} and other `nreps`
#' functions.
#'
#' @return a list object with a single field \code{$value}, containing a value
#' distributed according to \code{distribution.fun} and \code{distribution.pars}.
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @export

# TESTED
dummyalgo <- function(distribution.fun = "rnorm",
                      distribution.pars = list(mean = 0, sd = 1),
                      instance)
{
  # ========== Error catching ========== #
  assertthat::assert_that(
    is.character(distribution.fun) && length(distribution.fun) == 1,
    is.list(distribution.pars))
  # ==================================== #

  distribution.pars$n <- 1
  return(list(value = do.call(distribution.fun, args = distribution.pars)))
}


#' Dummy instance (for testing only) - a function that does nothing and returns
#' nothing
#' @export
# TESTED
dummyinstance <- function(){
  # do nothing
}

