#' Simulated annealing (for testing/examples)
#'
#' Adapted from stats::optim(). Check their documentation / examples for
#' details.
#'
#' @param Temp controls the "SANN" method. It is the starting temperature for
#'             the cooling schedule.
#' @param budget stop criterion: number of function evaluations to execute
#' @param instance an instance object (see [calc_nreps()] for details)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' instance <- list(FUN = "TSP.dist", mydist = datasets::eurodist)
#'
#' example_SANN(Temp = 2000, budget = 10000, instance = instance)
#' }
#'
#'

# TESTED: OK
example_SANN <- function(Temp, budget, instance){

  dist.mat <- as.matrix(instance$mydist)

  # Define perturbation function: 2-opt swap
  gen.seq <- function(x, ...){ #
    idx   <- seq(2, nrow(dist.mat) - 1)
    chgpt <- sample(idx, size = 2, replace = FALSE)

    tmp         <- x[chgpt[1]]
    x[chgpt[1]] <- x[chgpt[2]]
    x[chgpt[2]] <- tmp

    return(x)
  }

  # random initial solution
  sq <- sample.int(n = nrow(dist.mat), replace = FALSE)
  sq <- c(sq, sq[1])

  # run optimizer
  res <- stats::optim(sq, fn  = TSP.dist, gen.seq,
                      method  = "SANN",
                      mydist  = instance$mydist,
                      control = list(maxit  = budget,
                                     temp   = Temp,
                                     trace  = FALSE,
                                     REPORT = 500))
  return(res)
}
