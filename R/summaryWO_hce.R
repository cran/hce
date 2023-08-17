#' Win odds summary for `hce` objects
#'
#' @param x an `hce` object.
#' @param ... additional parameters.
#' @returns a list containing the summary of wins, losses, and ties. It contains the following named objects:
#' * summary a data frame containing number of wins, losses, and ties by treatment group and the overall number of comparisons.
#' * summary_by_GROUP (if `GROUP` variable is specified) a summary data frame by `GROUP`.
#' * WO calculated WO (win odds) and WP (win probability) and their standard errors.
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::summaryWO()], [hce::summaryWO.data.frame()] methods.
#' @examples
#' dat <- as_hce(HCE4)
#' summaryWO(dat, ref = "P")
summaryWO.hce <- function(x, ...){
  Args <- base::list(...)
  x <- as_hce(x)
  x <- base::as.data.frame(x)
  
  if(!is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if ("P" %in% unique(x$TRTP)) ref <- "P"
  else ref <- unique(x$TRTP)[1]

  summaryWO.data.frame(x = x, AVAL = "AVAL", TRTP = "TRTP", ref = ref, GROUP = "GROUP")
}
