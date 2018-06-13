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


#' Simulated annealing (for testing/examples)
#' Adapted from stats::optim
#' @param Temp controls the "SANN" method. It is the starting temperature for
#'             the cooling schedule.
#' @param budget stop criterion: total number of function evaluations
#' @param instance instance object (see [calc_nreps2()] for details)
#' @export
my.SANN <- function(Temp, budget, instance){

  dist.mat <- as.matrix(instance$mydist)

  # Define perturbation function: 2-opt swap
  gen.seq <- function(x, ...){ #
    idx   <- seq(2, nrow(dist.mat) - 1)
    chgpt <- sample(idx, size = 2, replace = FALSE)
    tmp <- x[chgpt[1]]
    x[chgpt[1]] <- x[chgpt[2]]
    x[chgpt[2]] <- tmp
    return(x)
  }

  # random initial solution
  sq <- sample.int(n = nrow(dist.mat), replace = FALSE)
  sq <- c(sq, sq[1])

  # run optimizer
  res <- stats::optim(sq, fn = TSP.dist, gen.seq, method = "SANN",
                      mydist = instance$mydist,
                      control = list(maxit = budget,
                                     temp = Temp,
                                     trace = FALSE,
                                     REPORT = 500))
  return(res)
}


#' TSP instance generator (for testing/examples)
#' Adapted from stats::optim
#' @param x a valid closed route for the TSP instance
#' @param mydist object of class _dist_ defining the TSP instance
#' @export
TSP.dist <- function(x, mydist){
  distmat <- as.matrix(mydist)
  x2      <- stats::embed(x, 2)
  y       <- sum(distmat[cbind(x2[, 2], x2[, 1])])

  return(y)
}
