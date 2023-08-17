#' A generic function for stratified win odds with adjustment
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list containing the stratified results and results by strata.
#' @export
#' @md
#' @seealso [hce::stratWO.data.frame()]  methods.
stratWO <- function(x, ...) {
  UseMethod("stratWO")
}