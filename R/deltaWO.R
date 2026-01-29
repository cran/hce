#' A generic function for calculating win odds based on a threshold
#'
#' @param x an object used to select a method.
#' @param delta a numeric threshold. 
#' @param ... further arguments passed to or from other methods.
#' @return a data frame containing calculated values.
#' @export
#' @md
#' @seealso [hce::deltaWO.adhce()] method.
#'
deltaWO <- function (x, delta, ...) 
{
  UseMethod("deltaWO")
}