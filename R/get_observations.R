#' Run an algorithm on a problem.
#'
#' Call algorithm routine for the solution of a problem instance
#'
#' @param instance a list object containing the definitions of the problem
#'    instance. See [calc_nreps()] for details.
#' @param algo a list object containing the definitions of the algorithm.
#'    See [calc_nreps()] for details.
#' @param n number of observations to generate.
#'
#' @return vector of observed performance values
#'
#' @seealso [calc_nreps()]
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br},
#' \email{f.campelo@@aston.ac.uk})
#'
#' @export
#'
#' @examples
#' # Make a dummy instance with a centered (zero-mean) exponential distribution:
#' instance <- list(FUN = "dummyinstance", distr = "rexp", rate = 5, bias = -1/5)
#'
#' # Simulate a dummy algorithm that has a uniform distribution of expected
#' # performance values, between -25 and 50.
#' algorithm <- list(FUN = "dummyalgo",
#'                  distribution.fun = "runif",
#'                  distribution.pars = list(min = -25, max = 50))
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
