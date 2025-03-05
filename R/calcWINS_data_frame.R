#' Win statistics calculation using a data frame
#'
#' @param x a data frame containing subject-level data.
#' @param AVAL variable in the data with ordinal analysis values.
#' @param TRTP the treatment variable in the data.
#' @param ref the reference treatment group.
#' @param alpha 2-sided significance level. The default is 0.05.
#' @param WOnull the null hypothesis. The default is 1.
#' @param SE_WP_Type biased or unbiased standard error for win probability. The default is biased.
#' @param ... additional parameters.
#' @details When `SE_WP_Type = "unbiased"`, the calculations for win proportion, net benefit, and win odds utilize the unbiased standard error from Brunner-Konietschke (2025) paper which is a reformulation of the original formula proposed by Bamber (1975).
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
#' @seealso [hce::calcWINS()], [hce::calcWINS.hce()], [hce::calcWINS.formula()].
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
#' calcWINS(x = COVID19b, AVAL = "GROUP", TRTP = "TRTP", ref = "Placebo")
#' ## Biased vs unbiased
#' n0 <- 10; n1 <- 20; p0 <- 0.2; p1 <- 0.5; x <- 1:20; delta <- 0.5
#' WP0 <- (p1 - p0)/2 + 0.5
#' DAT <- NULL
#' for(i in x){
#'   dat <- data.frame(AVAL = c(rbinom(n1, size = 1, p1), rbinom(n0, size = 1, p0)), 
#'   TRTP = c(rep("A", n1), rep("P", n0)))
#'   CL1 <- calcWINS(x = dat, AVAL = "AVAL", TRTP = "TRTP", ref = "P")$WP
#'   CL1$Type <- "biased"
#'   CL2 <- calcWINS(x = dat, AVAL = "AVAL", TRTP = "TRTP", 
#'                   ref = "P", SE_WP_Type = "unbiased")$WP
#'   CL2$Type <- "unbiased"
#'   DAT <- rbind(DAT, CL1, CL2)
#' }
#' WP <- DAT$WP[DAT$Type == "unbiased"]
#' plot(x, WP, pch = 19, xlab = "Simulations", ylab = "Win Probability", ylim = c(0., 1.))
#' points(x + delta, WP, pch = 19)
#' arrows(x, DAT$LCL[DAT$Type == "unbiased"], 
#'        x, DAT$UCL[DAT$Type == "unbiased"], angle = 90, code = 3, length = 0.05, "green")
#' arrows(x + delta, DAT$LCL[DAT$Type == "biased"], 
#'        x + delta, DAT$UCL[DAT$Type == "biased"], angle = 90, code = 3, length = 0.05, col = "red")
#' abline(h = WP0, col = "blue", lty = 3)
#' legend("bottomleft", legend = c("True WP", "Biased", "Unbiased"), 
#'                     col = c(4, 2, 3), lty = c(3, 1, 1 ), cex = 0.5)

calcWINS.data.frame <- function(x, AVAL, TRTP, ref, alpha = 0.05, WOnull = 1, SE_WP_Type = c("biased", "unbiased"), ...){
  SE_WP_Type <- match.arg(SE_WP_Type)
  data <- as.data.frame(x)
  alpha <- alpha[1]
  Ca <- stats::qnorm(1 - alpha/2)
  WOnull <- WOnull[1]
  WPnull <- WOnull/(WOnull + 1)
  gammanull <- (WOnull - 1)/(WOnull + 1)
  data$AVAL <- data[, base::names(data) == AVAL]
  data$TRTP <- data[, base::names(data) == TRTP]
  if (length(unique(data$TRTP)) != 2) 
    stop("The dataset should contain two treatment groups.")
  if (!ref %in% unique(data$TRTP)) 
    stop("Choose the reference from the values in TRTP.")
  data$TRTP <- base::ifelse(data$TRTP == ref, "P", "A")
  res0 <- calcWO(x = data, AVAL = "AVAL", TRTP = "TRTP", ref = "P", 
                 alpha = alpha, WOnull = WOnull)
  Res <- summaryWO(x = data, AVAL = "AVAL", TRTP = "TRTP", 
                   ref = "P", GROUP = "AVAL")
  res1 <- Res$summary_by_GROUP
  res2 <- table(data$TRTP, data$AVAL)
  res2 <- as.data.frame(res2)
  names(res2) <- c("TRTP", "GROUP", "N")
  res3 <- merge(res1, res2, by = c("TRTP", "GROUP"))
  res3$C <- ifelse(res3$TRTP == "A", res3$WIN/res3$N, res3$LOSS/res3$N)
  res3$D <- ifelse(res3$TRTP != "A", res3$WIN/res3$N, res3$LOSS/res3$N)
  P <- sum(res3$C * res3$N)
  Q <- sum(res3$D * res3$N)
  WR <- P/Q
  Pties <- Res$summary$TIE[1]/Res$summary$TOTAL[1]
  gamma <- (P - Q)/(P + Q)
  VAR <- 16/(P + Q)^4 * sum((res3$C * Q - res3$D * P)^2 * res3$N)
  gamma_SE <- sqrt(VAR)
  logWR_SE1 <- 2/(WR * (1 - gamma)^2) * gamma_SE
  logWR_SE2 <- 4/sqrt(3 * nrow(data)) * sqrt((1 + Pties)/(1 - 
                                                            Pties))
  threshold0 <- base::abs(gamma - gammanull)/gamma_SE
  P0 <- 2 * (1 - stats::pnorm(threshold0))
  threshold1 <- base::abs(log(WR) - log(WOnull))/logWR_SE1
  P1 <- 2 * (1 - stats::pnorm(threshold1))
  threshold2 <- base::abs(log(WR) - log(WOnull))/logWR_SE2
  P2 <- 2 * (1 - stats::pnorm(threshold2))
  
  if(SE_WP_Type == "unbiased"){
    nA <- sum(res2$N[res2$TRTP=="A"])
    nP <- sum(res2$N[res2$TRTP=="P"])
    SE_WP <- sqrt(res0$SE_WP^2 - (res0[, "WP"]*(1 - res0[, "WP"]) - 0.25*Pties)/(nA*nP))
    threshold_WP <- base::abs(res0[, "WP"] - WPnull)/SE_WP
    P_WP <- 2 * (1 - stats::pnorm(threshold_WP))
  } else {
    SE_WP <- res0[, "SE_WP"]
    threshold_WP <- base::abs(res0[, "WP"] - WPnull)/SE_WP
    P_WP <- 2 * (1 - stats::pnorm(threshold_WP))
  }
  out <- list()
  out$summary <- data.frame(WIN = P/2, LOSS = Q/2, TIE = Res$summary$TIE[1], 
                            TOTAL = Res$summary$TOTAL[1], Pties = Pties)
  out$WP <- data.frame(WP = res0[, "WP"], LCL = res0[, "WP"] - Ca * SE_WP, UCL = res0[, "WP"] + Ca * SE_WP, Pvalue = P_WP)
  out$NetBenefit <- data.frame(NetBenefit = 2 * res0[, "WP"] - 1, 
                               LCL = 2 * (res0[, "WP"] - Ca * SE_WP) - 1, 
                               UCL = 2 * (res0[, "WP"] + Ca * SE_WP) - 1, Pvalue = P_WP)
  out$WO <- res0[, c("WO", "LCL", "UCL", "Pvalue")]
  out$WR1 <- data.frame(WR = WR, LCL1 = WR * exp(-Ca * logWR_SE1), 
                        UCL1 = WR * exp(Ca * logWR_SE1), Pvalue1 = P1)
  out$WR2 <- data.frame(WR = WR, LCL2 = WR * exp(-Ca * logWR_SE2), 
                        UCL2 = WR * exp(Ca * logWR_SE2), Pvalue2 = P2)
  out$gamma <- data.frame(gamma = gamma, LCL = gamma - Ca * 
                            gamma_SE, UCL = gamma + Ca * gamma_SE, Pvalue = P0)
  out$SE <- data.frame(WP_SE = res0[, "SE_WP"], NetBenefit_SE = 2 * 
                         res0[, "SE_WP"], logWR_SE1 = logWR_SE1, logWR_SE2 = logWR_SE2, 
                       gamma_SE = gamma_SE)
  return(out)
}

