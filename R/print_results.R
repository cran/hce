
#' A print method for `hce_results` objects
#'
#' @param x an object of class `hce_results`.
#' @param ... additional arguments to be passed to [base::print()] function.
#' @return no return value, called for printing.
#' @export
#'
#' @examples
#' print(powerWO(N = 1000, WO = 1.2))
print.hce_results <- function(x, ...){

  base::print(x$result, ...)

}
