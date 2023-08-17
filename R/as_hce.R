#' A generic function for coercing data structures to `hce` objects
#'
#' @param x an object used to select a method.
#' @param ... additional parameters.
#'
#' @return an `hce` object.
#' @export
#' @md
#' @seealso [hce::as_hce.data.frame()].
#' @examples
#' ### data frames 
#' data(HCE1)
#' HCE <- as_hce(HCE1)
#' calcWINS(HCE)
as_hce <- function(x, ...) {
  UseMethod("as_hce")
}
