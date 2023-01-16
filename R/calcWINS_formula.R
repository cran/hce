#' Win statistics calculation using formula syntax
#'
#' @param x an object of class formula.
#' @param data a data frame.
#' @param ... additional parameters.
#'
#' @returns a list containing win statistics and their confidence intervals. It contains the following named data frames: 
#' * summary a data frame containing number of wins, losses, and ties of the active treatment group and the overall number of comparisons. 
#' * WP a data frame containing the win probability and its confidence interval. 
#' * NetBenefit a data frame containing the net benefit and its confidence interval. This is just a `2x-1` transformation of WP and its CI.
#' * WO a data frame containing the win odds and its confidence interval. 
#' * WR1 a data frame containing the win ratio and its confidence interval, using the transformed standard error of the `gamma` statistic. 
#' * WR2 a data frame containing the win ratio and its confidence interval, using the standard error calculated using `Pties`. 
#' * gamma a data frame containing Goodman Kruskal's `gamma` and its confidence interval. 
#' * SE a data frame containing standard errors used to calculed the Confidence intervals for win statistics. 
#' @export
#' @md
#' @seealso [hce::calcWINS()], [hce::calcWINS.hce()], [hce::calcWINS.data.frame()].
#' @references
#' The theory of win statistics is covered in the following papers. 
#' * For the win proportion CI calculation see 
#' \cr \cr Gasparyan, Samvel B., et al. "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2 (2021): 580-611 <doi:10.1177/0962280220942558>.
#' * The win odds CI is calculated using the formula in 
#' \cr \cr Gasparyan, Samvel B., et al. "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6 (2021): 765-787 <doi:10.1080/10543406.2021.1968893>.
#' * The win ratio the first CI uses the standard error derived from the standard error of the `gamma` statistic.
#' * The win ratio the second CI uses the standard error presented in
#' \cr \cr Yu RX, Ganju J. Sample size formula for a win ratio endpoint. Statistics in medicine. 2022 Mar 15;41(6):950-63 <doi:10.1002/sim.9297>.
#' * The Goodman Kruskal's `gamma` and its CI match those in DescTools::GoodmanKruskalGamma() and are based on
#' \cr \cr Agresti, A. (2002) Categorical Data Analysis. John Wiley & Sons, pp. 57-59.
#' \cr \cr Brown, M.B., Benedetti, J.K.(1977) Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables, Journal of the American Statistical Association, 72, 309-315.
#' \cr \cr Goodman, L. A., & Kruskal, W. H. (1954) Measures of association for cross classifications. Journal of the American Statistical Association, 49, 732-764. 
#' \cr \cr Goodman, L. A., & Kruskal, W. H. (1963) Measures of association for cross classifications III: Approximate sampling theory. Journal of the American Statistical Association, 58, 310-364.
#' @examples
#' # Example 1
#' calcWINS(x = GROUP ~ TRTP, data = COVID19b)
#' # Example 2
#' calcWINS(x = GROUP ~ TRTP, data = COVID19, ref = "Placebo", alpha = 0.01, WOnull = 1.2)
calcWINS.formula <- function(x, data, ...){
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
  
  res <- calcWINS.data.frame(x = mf, AVAL = formulavars[1], TRTP = formulavars[2], 
                             ref = ref, alpha = alpha, WOnull = WOnull)
  res$formula <- base::deparse(formula0)
  res$ref <- base::paste(Level[Level != ref],"vs", ref)
  res$Input <- data.frame(alpha = alpha, WOnull = WOnull)
  res
}