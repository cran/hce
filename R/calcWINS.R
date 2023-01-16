#' A generic function for calculating win statistics
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a data frame containing calculated values.
#' @export
#' @md
#' @seealso [hce::calcWINS.hce()], [hce::calcWINS.formula()], [hce::calcWINS.data.frame()]  methods.
calcWINS <- function(x, ...) {
  UseMethod("calcWINS")
}
