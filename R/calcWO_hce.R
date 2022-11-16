#' Win odds calculation for `hce` objects
#'
#' @param x an `hce` object.
#' @param ... additional parameters.
#' @returns a data frame containing the win odds and its confidence interval. It contains the following columns:
#' * WO calculated win odds.
#' * LCL lower confidence limit.
#' * UCL upper confidence limit.
#' * SE standard error of the win odds.
#' * WOnull win odds of the null hypothesis (specified in the `WOnull` argument).
#' * alpha two-sided significance level for calculating the confidence interval (specified in the `alpha` argument).
#' * Pvalue p-value associated with testing the null hypothesis.
#' * WP calculated win probability.
#' * WP_SE standard error of the win probability.
#' @export
#' @examples
#' Rates_A <- c(1, 1.5) 
#' Rates_P <- c(2, 2) 
#' dat <- simHCE(n = 500, TTE_A = Rates_A, TTE_P = Rates_P, CM_A = 1.25, CM_P = 1)
#' calcWO(dat)
#' calcWO(dat, ref = "A")
calcWO.hce <- function(x, ...){
  Args <- base::list(...)
  x <- base::as.data.frame(x)

  if(!is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if ("P" %in% unique(x$TRTP)) ref <- "P"
  else ref <- unique(x$TRTP)[1]
  

  calcWO.data.frame(x = x, AVAL = "AVAL", TRTP = "TRTP", ref = ref)
}
