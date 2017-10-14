#' Run an algorithm on a problem.
#'
#' Call algorithm routine for the solution of a problem instance
#'
#' @param instance a list object containing the definitions of the problem
#'    instance. See [run_nreps2()] for details.
#' @param algo a list object containing the definitions of the algorithm.
#'    See [run_nreps2()] for details.
#' @param n number of observations to generate.
#'
#' @return vector of observed performance values
#'
#' @seealso \link{run_nreps2}
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'
#' @examples
#' algorithm <- list(FUN = "dummyalgo", alias = "myalgo",
#'                   distribution.fun = "rnorm",
#'                   distribution.pars = list(mean = 50, sd = 10))
#' instance <- list(FUN = "dummyinstance")
#' x <- get_observations(algorithm, instance, n = 1000)
#' hist(x)

# TESTED
get_observations <- function(algo,        # algorithm parameters
                             instance,    # problem parameters
                             n = 1)       # number of observations to generate.
{

  # ========== Error catching ========== #
  # Most of error catching is already performed by the calling routine
  # run_nreps2(), so no need to do much here, except:
  assertthat::assert_that(assertthat::is.count(n))
  # ==================================== #


  # remove '$FUN' field from list of arguments
  # and include the problem definition as field 'instance'
  myargs          <- algo[names(algo) != "FUN"]
  myargs          <- myargs[names(myargs) != "alias"]
  myargs$instance <- instance

  # Get observation(s)
  f <- numeric(n)
  for (i in 1:n){
    result <- do.call(algo$FUN,
                      myargs)
    f[i] <- result$value
  }
  return(f)
}
