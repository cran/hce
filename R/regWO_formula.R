#' Win Odds Regression Using a Formula Syntax
#'
#' This function performs regression analysis for the win odds using a single numeric covariate.
#' 
#' @param x an object of class formula.
#' @param data a data frame.
#' @param ... additional parameters.
#' @returns a data frame containing the calculated win odds and its confidence interval, including: 
#' * WO_beta adjusted win odds.
#' * LCL lower confidence limit for adjusted WO.
#' * UCL upper confidence limit for adjusted WO.
#' * SE standard error of the adjusted win odds.
#' * WOnull win odds of the null hypothesis (specified in the `WOnull` argument).
#' * alpha two-sided significance level for calculating the confidence interval (specified in the `alpha` argument).
#' * Pvalue p-value associated with testing the null hypothesis.
#' * N total number of patients in the analysis.
#' * beta adjusted win probability.
#' * LCL_beta lower confidence limit for adjusted win probability.
#' * UCL_beta upper confidence limit for adjusted win probability.
#' * SE_beta standard error for the adjusted win probability.
#' * SD_beta standard deviation for the adjusted win probability.
#' * WP (non-adjusted) win probability.
#' * SE_WP standard error of the non-adjusted win probability.
#' * SD_WP standard deviation of the non-adjusted win probability.
#' * WO non-adjusted win odds.
#' * COVAR_MEAN_DIFF mean difference between two treatment groups of the numeric covariate.
#' * COVAR_VAR sum of variances of two treatment groups of the numeric covariate.
#' * COVAR_COV covariance between the response and the numeric covariate.
#' * formula returning the specified formula in the `x` argument.
#' * ref showing how the reference group was selected. Can be modifying by specifying the `ref` argument.
#' @export
#' @md
#' @seealso [hce::regWO()], [hce::regWO.data.frame()].
#' @references Gasparyan SB et al. (2021) "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2: 580-611. <doi:10.1177/0962280220942558>.
#' @examples
#' regWO(AVAL ~ TRTP, data = KHCE)
#' regWO(AVAL ~ TRTP + EGFRBL, data = KHCE)
regWO.formula <- function (x, data, ...) 
{ # Extra arguments to modify the default values of the data frame call
  Args <- base::list(...)
  # The names in the formula x
  formulavars <- base::all.vars(x)
  # Recreate a formula from a character vector by dropping unnecessary terms
  if (length(formulavars) > 2){
    formula0 <- stats::reformulate(formulavars[2:3], response = formulavars[1])
  } else {
    formula0 <- stats::reformulate(formulavars[2], response = formulavars[1])
  }
  # Subsets the data frame to keep only the variables in the formula
  mf <- stats::model.frame(formula = formula0, data = data)
  if (length(formulavars) > 2){
    Level <- base::unique(mf[[formulavars[2]]])
    if (!base::is.null(Args[["ref"]])) 
      ref <- Args[["ref"]]
    else if (base::all(Level == c("A", "P"))) 
      ref <- "P"
    else ref <- Level[1]
    if (!base::is.null(Args[["alpha"]])) 
      alpha <- Args[["alpha"]]
    else alpha <- 0.05
    if (!base::is.null(Args[["WOnull"]])) 
      WOnull <- Args[["WOnull"]]
    else WOnull <- 1
    res <- regWO.data.frame(x = mf, AVAL = formulavars[1], TRTP = formulavars[2], COVAR = formulavars[3], 
                 ref = ref, alpha = alpha, WOnull = WOnull)
    res$formula <- base::deparse(formula0)
    res$ref <- base::paste(Level[Level != ref], "vs", ref)
  } else {
    res <- calcWO.formula(x = formula0, data = data, ...)
    res$formula <- base::deparse(formula0)
  }
  return(res)
}
