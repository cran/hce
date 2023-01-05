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
#' * WP_SD standard deviation of the win probability, calculated as `WP_SE` multiplied by `sqrt(N)`.
#' * N total number of patients in the analysis.
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::calcWO.formula()], [hce::calcWO.data.frame()].
#' @examples
#' Rates_A <- c(1, 1.5) 
#' Rates_P <- c(2, 2) 
#' dat <- simHCE(n = 500, TTE_A = Rates_A, TTE_P = Rates_P, CM_A = 1.25, CM_P = 1)
#' calcWO(dat)
#' calcWO(dat, ref = "A", WOnull = 1, alpha = 0.01)
calcWO.hce <- function(x, ...){
  Args <- base::list(...)
  x <- new_hce(x)
  x <- base::as.data.frame(x)

  if(!is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if ("P" %in% unique(x$TRTP)) ref <- "P"
  else ref <- unique(x$TRTP)[1]
  if(!base::is.null(Args[["alpha"]])) alpha <- Args[["alpha"]]
  else alpha <- 0.05
  if(!base::is.null(Args[["WOnull"]])) WOnull <- Args[["WOnull"]]
  else WOnull <- 1
  

  calcWO.data.frame(x = x, AVAL = "AVAL", TRTP = "TRTP", 
                    ref = ref, WOnull = WOnull, alpha = alpha)
}
