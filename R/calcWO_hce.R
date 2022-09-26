#' Win odds calculation for an hce object
#'
#' @param x an hce object.
#' @param ... additional parameters.
#' @returns a data frame containing the win odds and its confidence interval. It contains the following columns:
#' * WO calculated win odds.
#' * LCL lower confidence limit.
#' * UCL upper confidence limit.
#' * SE standard error of the win odds.
#' * WOnull win odds of the null hypothesis (specified in the `WOnull` argument).
#' * alpha two-sided significance level for calculating the confidence interval (specified in the `alpha` argument).
#' * Pvalue p-value associated with testing the null hypothesis.
#' @export
#' @examples
#' data(HCE3)
#' class(HCE3) <- c("hce", "data.frame")
#' calcWO(HCE3)
#'
calcWO.hce <- function(x, ...){
  Args <- base::list(...)
  x <- base::as.data.frame(x)

  if(!is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else ref <- "P"
  if(! ref %in% c("A", "P")) stop("Choose the reference from the values A or P.")

  calcWO.data.frame(x = x, AVAL = "AVAL", TRTP = "TRTP", ref = ref)
}
