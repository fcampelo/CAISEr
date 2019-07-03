#' TSP instance generator (for testing/examples)
#'
#' Adapted from stats::optim(). Check their documentation / examples for
#' details.
#'
#' @param x a valid closed route for the TSP instance
#' @param mydist object of class _dist_ defining the TSP instance
#'
#' @export

# TESTED: OK
TSP.dist <- function(x, mydist){
  distmat <- as.matrix(mydist)
  x2      <- stats::embed(x, 2)
  y       <- sum(distmat[cbind(x2[, 2], x2[, 1])])

  return(y)
}
