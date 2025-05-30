#' Win odds calculation using formula syntax
#'
#' @param x an object of class formula.
#' @param data a data frame.
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
#' * LCL_WP lower confidence limit for `WP`.
#' * UCL_WP upper confidence limit for `WP`.
#' * SE_WP standard error of the win probability.
#' * SD_WP standard deviation of the win probability, calculated as `SE_WP` multiplied by `sqrt(N)`.
#' * N total number of patients in the analysis.
#' * formula returning the specified formula in the `x` argument.
#' * ref showing how the reference group was selected. Can be modifying by specifying the `ref` argument.
#' @export
#' @md
#' @seealso [hce::calcWO()], [hce::calcWO.hce()], [hce::calcWO.data.frame()].
#' @references Gasparyan SB et al. "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2 (2021): 580-611. <doi:10.1177/0962280220942558>
#' @examples
#' #Example 1
#' data(HCE1)
#' calcWO(AVAL ~ TRTP, data = HCE1)
#'
#'#Example 2
#' calcWO(GROUP ~ TRTP, data = COVID19, ref = "Placebo", alpha = 0.01)
#'
calcWO.formula <- function(x, data, ...){
  Args <- base::list(...)
  formulavars <- base::all.vars(x)
  formula0 <- stats::reformulate(formulavars[2], response = formulavars[1])
  mf <- stats::model.frame(formula = formula0, data = data)
  Level <- base::unique(mf[[formulavars[2]]])
  Levellength <- base::length(Level)

    if(Levellength != 2){
    message1 <- base::paste("The variable", formulavars[2], "should have exactly 2 unique values, but has", Levellength)
    stop(message1)
  }
  if(!base::is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if (base::all(Level == c("A", "P"))) ref <- "P"
  else ref <- Level[1]
  if(! ref %in% Level) stop(base::paste("Choose the reference from the values",
                                  base::paste(Level, collapse = ", ")))

  if(!base::is.null(Args[["alpha"]])) alpha <- Args[["alpha"]]
  else alpha <- 0.05
  if(!base::is.null(Args[["WOnull"]])) WOnull <- Args[["WOnull"]]
  else WOnull <- 1
  
  res <- calcWO.data.frame(x = mf, AVAL = formulavars[1], TRTP = formulavars[2], 
                           ref = ref, alpha = alpha, WOnull = WOnull)
  res$formula <- base::deparse(formula0)
  res$ref <- base::paste(Level[Level != ref],"vs", ref)
  res
}
