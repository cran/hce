#' A generic function for calculating win odds
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @return a data frame containing calculated values.
#' @export
#' @md
#' @seealso [hce::calcWO.hce()], [hce::calcWO.formula()], [hce::calcWO.data.frame()]  methods.
#'
calcWO <- function(x, ...) {
  UseMethod("calcWO")
}
