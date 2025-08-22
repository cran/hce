#' Win odds summary for `adhce` objects
#'
#' @param x an `adhce` object.
#' @param ... additional parameters.
#' @returns a list containing the summary of wins, losses, and ties. It contains the following named objects:
#' * summary a data frame containing number of wins, losses, and ties by treatment group and the overall number of comparisons.
#' * summary_by_GROUP (if `GROUP` variable is specified) a summary data frame by `GROUP`.
#' * WO calculated WO (win odds) and WP (win probability) and their standard errors.
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::summaryWO()], [hce::summaryWO.data.frame()], [hce::summaryWO.formula()], [hce::summaryWO.hce()] methods.
#' @examples
#' dat <- as_hce(HCE4)
#' ## `PADY` is not present in the dataset, hence converts it to an `hce` object 
#' ## instead of an `adhce` object.
#' class(dat)
#' summaryWO(dat, ref = "P")
#' ## The class is `adhce` hence will use the variable `GROUP`.
#' HCE4$PADY <- 1080
#' dat <- as_hce(HCE4) 
#' class(dat)
#' summaryWO(dat, ref = "P")
summaryWO.adhce <- function(x, ...){
  Args <- base::list(...)
  x <- as_hce(x)
  x <- base::as.data.frame(x)
  if(!is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if ("P" %in% unique(x$TRTP)) ref <- "P"
  else ref <- unique(x$TRTP)[1]
  summaryWO.data.frame(x = x, AVAL = "AVAL", TRTP = "TRTP", ref = ref, GROUP = "GROUP")
}
