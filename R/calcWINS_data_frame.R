#' Win statistics calculation using a data frame
#'
#' @param x a data frame containing subject-level data.
#' @param AVAL variable in the data with ordinal analysis values.
#' @param TRTP the treatment variable in the data.
#' @param ref the reference treatment group.
#' @param alpha 2-sided significance level. The default is 0.05.
#' @param WOnull the null hypothesis. The default is 1.
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
#' @seealso [hce::calcWINS()], [hce::calcWINS.hce()], [hce::calcWINS.formula()].
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
#' calcWINS(x = COVID19b, AVAL = "GROUP", TRTP = "TRTP", ref = "Placebo")
calcWINS.data.frame <- function(x, AVAL, TRTP, ref, alpha = 0.05, WOnull = 1, ...){
  
  data <- as.data.frame(x)
  alpha <- alpha[1]
  Ca <- stats::qnorm(1 - alpha/2)
  WOnull <- WOnull[1]
  WPnull <- WOnull/(WOnull + 1)
  gammanull <- (WOnull - 1)/(WOnull + 1)
  
  data$AVAL <- data[, base::names(data) == AVAL]
  data$TRTP <- data[, base::names(data) == TRTP]
  if(length(unique(data$TRTP)) != 2) stop("The dataset should contain two treatment groups.")
  if(!ref %in% unique(data$TRTP)) stop("Choose the reference from the values in TRTP.")
  data$TRTP <- base::ifelse(data$TRTP == ref, "P", "A")
  
  res0 <- calcWO(x = data, AVAL = "AVAL", TRTP = "TRTP", ref = "P", alpha = alpha, WOnull = WOnull)
  Res <- summaryWO(x = data, AVAL = "AVAL", TRTP = "TRTP", ref = "P", GROUP = "AVAL")
  res1 <- Res$summary_by_GROUP
  res2 <- table(data$TRTP, data$AVAL)
  res2 <- as.data.frame(res2)
  names(res2) <- c("TRTP", "GROUP", "N")
  res3 <- merge(res1, res2, by = c("TRTP", "GROUP"))
  res3$C <- ifelse(res3$TRTP == "A", res3$WIN/res3$N, res3$LOSS/res3$N)
  res3$D <- ifelse(res3$TRTP != "A", res3$WIN/res3$N, res3$LOSS/res3$N)
  
  P <- sum(res3$C*res3$N)
  Q <- sum(res3$D*res3$N)
  WR <- P/Q
  Pties <- Res$summary$TIE[1]/Res$summary$TOTAL[1]
  gamma <- (P - Q)/(P + Q)
  VAR <- 16/(P + Q)^4*sum((res3$C*Q - res3$D*P)^2*res3$N)
  gamma_SE <- sqrt(VAR)
  logWR_SE1 <- 2/(WR*(1 - gamma)^2)*gamma_SE 
  logWR_SE2 <- 4/sqrt(3*nrow(data)) *sqrt((1 + Pties)/(1 - Pties))
  
 
  threshold_WP <- base::abs(res0[, "WP"] - WPnull)/res0[, "SE_WP"]
  P_WP <- 2*(1 - stats::pnorm(threshold_WP))
  threshold0 <- base::abs(gamma - gammanull)/gamma_SE
  P0 <- 2*(1 - stats::pnorm(threshold0))
  threshold1 <- base::abs(log(WR) - log(WOnull))/logWR_SE1
  P1 <- 2*(1 - stats::pnorm(threshold1))
  threshold2 <- base::abs(log(WR) - log(WOnull))/logWR_SE2
  P2 <- 2*(1 - stats::pnorm(threshold2))
  
  out <- list()
  out$summary <- data.frame(WIN = P/2, LOSS = Q/2, TIE = Res$summary$TIE[1], 
                            TOTAL = Res$summary$TOTAL[1], Pties = Pties)
  out$WP <- data.frame(WP = res0[, "WP"], LCL = res0[, "WP"] - Ca*res0[, "SE_WP"], 
                       UCL = res0[, "WP"] + Ca*res0[, "SE_WP"], Pvalue = P_WP)
  out$NetBenefit <- data.frame(NetBenefit = 2*res0[, "WP"] - 1, 
                               LCL = 2*(res0[, "WP"] - Ca*res0[, "SE_WP"]) - 1, 
                               UCL = 2*(res0[, "WP"] + Ca*res0[, "SE_WP"]) - 1, Pvalue = P_WP)
  out$WO <- res0[, c("WO", "LCL", "UCL", "Pvalue")]
  out$WR1 <- data.frame(WR = WR, LCL1 = WR*exp(- Ca*logWR_SE1), UCL1 = WR*exp(Ca*logWR_SE1), Pvalue1 = P1)
  out$WR2 <- data.frame(WR = WR, LCL2 = WR*exp(- Ca*logWR_SE2), UCL2 = WR*exp(Ca*logWR_SE2), Pvalue2 = P2)
  out$gamma <- data.frame(gamma = gamma, 
                          LCL = gamma - Ca*gamma_SE, UCL = gamma + Ca*gamma_SE, Pvalue = P0)
  
  
  out$SE <- data.frame(WP_SE = res0[, "SE_WP"], NetBenefit_SE = 2*res0[, "SE_WP"],
                       logWR_SE1 = logWR_SE1, logWR_SE2 = logWR_SE2, 
                       gamma_SE = gamma_SE)
  return(out)
}

