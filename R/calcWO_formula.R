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
#' @export
#' @examples
#' data(HCE1)
#' calcWO(AVAL ~ TRTP, data = HCE1)
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

  res <- calcWO.data.frame(x = mf, AVAL = formulavars[1], TRTP = formulavars[2], ref = ref)
  res$formula <- base::deparse(formula0)
  res$ref <- base::paste(Level[Level != ref],"vs", ref)
  res
}
