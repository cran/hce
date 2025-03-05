#' Win statistics calculation for `hce` objects
#'
#' @param x an `hce` object.
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
#' * SE a data frame containing standard errors used to calculated the Confidence intervals for win statistics. 
#' @export
#' @md
#' @seealso [hce::calcWINS()], [hce::calcWINS.formula()], [hce::calcWINS.data.frame()].
#' @references
#' The theory of win statistics is covered in the following papers: 
#' * Win proportion and win odds confidence interval calculation: 
#' \cr \cr Bamber D (1975) "The area above the ordinal dominance graph and the area below the receiver operating characteristic graph." Journal of Mathematical Psychology 12.4: 387-415. <doi:10.1016/0022-2496(75)90001-2>.
#' \cr \cr DeLong ER et al. (1988) "Comparing the Areas Under Two or More Correlated Receiver Operating Characteristic Curves: A Nonparametric Approach." Biometrics 44.3: 837-845. <doi:10.2307/2531595>.
#' \cr \cr Brunner E et al. (2021) "Win odds: an adaptation of the win ratio to include ties." Statistics in Medicine 40.14: 3367-3384. <doi:10.1002/sim.8967>.
#' \cr \cr Gasparyan SB et al. (2021) "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2: 580-611. <doi:10.1177/0962280220942558>.
#' \cr \cr Gasparyan SB et al. (2021) "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6: 765-787. <doi:10.1080/10543406.2021.1968893>.
#' \cr \cr Brunner E, Konietschke F. (2025) "An unbiased rank-based estimator of the Mann–Whitney variance including the case of ties." Statistical Papers 66.20. <doi:10.1007/s00362-024-01635-0>.
#' * Win ratio: the first CI utilizes the standard error derived from the `gamma` statistic standard error as outlined by:
#' \cr \cr Gasparyan SB, Kowalewski EK, Buenconsejo J, Koch GG. (2023) "Hierarchical Composite Endpoints in COVID-19: The DARE-19 Trial." In Case Studies in Innovative Clinical Trials, Chapter 7, 95–148. Chapman; Hall/CRC. <doi:10.1201/9781003288640-7>.
#' * Win ratio: the second CI utilizes the standard error presented by:
#' \cr \cr Yu RX, Ganju J. (2022) "Sample size formula for a win ratio endpoint." Statistics in Medicine 41.6: 950-63. <doi:10.1002/sim.9297>.
#' * Goodman Kruskal's `gamma` and CI: matches implementation in `DescTools::GoodmanKruskalGamma()` and based on:
#' \cr \cr Agresti A. (2002) Categorical Data Analysis. John Wiley & Sons, pp. 57-59. <doi:10.1002/0471249688>.
#' \cr \cr Brown MB, Benedetti JK. (1977) "Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables." Journal of the American Statistical Association 72, 309-315. <doi:10.1080/01621459.1977.10480995>.
#' \cr \cr Goodman LA, Kruskal WH. (1954) "Measures of association for cross classifications." Journal of the American Statistical Association 49, 732-764. <doi:10.1080/01621459.1954.10501231>.
#' \cr \cr Goodman LA, Kruskal WH. (1963) "Measures of association for cross classifications III: Approximate sampling theory." Journal of the American Statistical Association 58, 310-364. <doi:10.1080/01621459.1963.10500850>.
#' @examples
#' # Example 1
#' COVID19HCE <- hce(GROUP = COVID19$GROUP, TRTP = COVID19$TRTP)
#' calcWINS(COVID19HCE)
#' # Example 2
#' COVID19bHCE <- hce(GROUP = COVID19b$GROUP, TRTP = COVID19b$TRTP)
#' calcWINS(COVID19bHCE, ref = "Placebo", WOnull = 1.1, alpha = 0.01)
#' # Example 3
#' calcWINS(COVID19HCE, SE_WP_Type = "unbiased")$WP
#' calcWINS(COVID19HCE, SE_WP_Type = "biased")$WP
calcWINS.hce <- function(x, ...){
  Args <- base::list(...)
  x <- as_hce(x)
  x <- base::as.data.frame(x)
  
  if(!is.null(Args[["ref"]])) ref <- Args[["ref"]]
  else if ("P" %in% unique(x$TRTP)) ref <- "P"
  else ref <- unique(x$TRTP)[1]
  if(!base::is.null(Args[["alpha"]])) alpha <- Args[["alpha"]]
  else alpha <- 0.05
  if(!base::is.null(Args[["WOnull"]])) WOnull <- Args[["WOnull"]]
  else WOnull <- 1
  
  res <- calcWINS.data.frame(x = x, AVAL = "AVAL", TRTP = "TRTP", 
                             ref = ref, alpha = alpha, WOnull = WOnull, SE_WP_Type = Args[["SE_WP_Type"]])
  res$ref <- base::paste(unique(x$TRTP)[unique(x$TRTP) != ref],"vs", ref)
  res$Input <- data.frame(alpha = alpha, WOnull = WOnull)
  res
}