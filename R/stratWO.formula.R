#' Stratified win odds with adjustment using formula
#'
#' @param x an object of class formula.
#' @param data a data frame.
#' @param ... additional parameters.
#' @return a data frame containing the following columns:
#' * WO stratified (or adjusted/stratified) win odds.
#' * LCL lower confidence limit for adjusted (or adjusted/stratified) WO.
#' * UCL upper confidence limit for adjusted (or adjusted/stratified) WO.
#' * SE standard error of the adjusted (or adjusted/stratified) win odds.
#' * WOnull win odds of the null hypothesis (specified in the `WOnull` argument).
#' * alpha two-sided significance level for calculating the confidence interval (specified in the `alpha` argument).
#' * Pvalue p-value associated with testing the null hypothesis.
#' * WP adjusted (or adjusted/stratified) win probability.
#' * LCL_WP lower confidence limit for adjusted (or adjusted/stratified) WP.
#' * UCL_WP upper confidence limit for adjusted (or adjusted/stratified) WP.
#' * SE_WP standard error for the adjusted (or adjusted/stratified) win probability.
#' * SD_WP standard deviation of the adjusted (or adjusted/stratified) win probability.
#' * N total number of patients in the analysis.
#' * Type "STRATIFIED" or "STRATIFIED/ADJUSTED" depending on whether `COVAR` is specified.
#' @export
#' @md
#' @seealso [hce::stratWO()], [hce::stratWO.data.frame()].
#' @references Gasparyan SB et al. (2021) "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2: 580-611. <doi:10.1177/0962280220942558>.
#' @examples
#' # Example 1: Stratified win odds with covariate adjustment
#' stratWO(x = AVAL ~ TRTP + EGFRBL | STRATAN, data = KHCE)
#' # Example 2: Stratified win odds without covariate adjustment
#' stratWO(x = AVAL ~ TRTP | STRATAN, data = KHCE)
#' # Example 3: Only covariate adjustment (without stratification) is implemented in regWO()
#' regWO(x = AVAL ~ TRTP + EGFRBL, data = KHCE)
#' # Example 4: Non-standardized names
#' dat <- data.frame(response = KHCE$AVAL, treatment = KHCE$TRTP, 
#' covariate = KHCE$EGFRBL, group = KHCE$STRATAN)
#' stratWO(x = response ~ treatment + covariate | group, data = dat)
stratWO.formula <- function(x, data, ...){
  Args <- base::list(...)
  x <- as_formulae(x)
  stopifnot("The treatment and response variables must be in the dataset." = base::all(base::c(x$AVAL, x$TRTP) %in% base::names(data)))
  Level <- base::unique(data[, x$TRTP])
  Levellength <- base::length(Level)
  
  if (Levellength != 2) {
    message1 <- base::paste("The variable", x$TRTP, 
                            "should have exactly 2 unique values, but has", Levellength)
    stop(message1)
  }
  stopifnot("The stratification must be specified using the conditioning `|`." =
              !base::is.null(x$GROUP))
  if (!base::is.null(Args[["ref"]])) 
    ref <- Args[["ref"]]
  else if (base::all(base::sort(Level) == c("A", "P"))) 
    ref <- "P"
  else ref <- Level[1]
  if (!ref %in% Level) 
    stop(base::paste("Choose the reference from the values", 
                     base::paste(Level, collapse = ", ")))

  res <- stratWO.data.frame(x = data, AVAL = x$AVAL, 
                            TRTP = x$TRTP, ref = ref, 
                            STRATA = x$GROUP, COVAR = x$COVAR)
  res$ref <- base::paste(Level[Level != ref], "vs", ref)
  res$formula <- x$formula
  res
}


