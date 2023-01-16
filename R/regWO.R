#' A generic function for win odds regression
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a data frame containing calculated values.
#' @export
#' @md
#' @seealso [hce::regWO.data.frame()]  methods.
regWO <- function(x, ...) {
  UseMethod("regWO")
}