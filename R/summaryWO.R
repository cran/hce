#' A generic function for summarizing win odds
#'
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @return a data frame containing calculated values.
#' @export
#' @md
#' @seealso [hce::summaryWO.data.frame()], [hce::summaryWO.hce()]  methods.
#'
summaryWO <- function(x, ...) {
  UseMethod("summaryWO")
}
