
#' Win odds calculation using a data frame
#'
#' @param x a data frame containing subject-level data.
#' @param AVAL variable in the data with ordinal analysis values
#' @param TRTP the treatment variable in the data.
#' @param ref the reference treatment group.
#' @param alpha significance level. The default is 0.05.
#' @param WOnull the null hypothesis. The default is 1.
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
#' @md
#' @references Gasparyan, Samvel B., et al. "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2 (2021): 580-611.
#' @examples
#' data(HCE4)
#' calcWO(x = HCE4, AVAL = "AVAL", TRTP = "TRTP", ref = "P")
calcWO.data.frame <- function(x, AVAL, TRTP, ref, alpha = 0.05, WOnull = 1, ...){

  data <- as.data.frame(x)
  alpha <- alpha[1]
  WOnull <- WOnull[1]
  WPnull <- WOnull/(WOnull + 1)


  data$AVAL <- data[, base::names(data)== AVAL]
  data$TRTP <- data[, base::names(data)== TRTP]
  if(length(unique(data$TRTP)) !=2) stop("The dataset should contain two treatment groups")
  data$TRTP <- base::ifelse(data$TRTP == ref, "P", "A")

  A <- base::rank(c(data$AVAL[data$TRTP=="A"], data$AVAL[data$TRTP=="P"]), ties.method = "average")
  B <- base::tapply(data$AVAL, data$TRTP, base::rank, ties.method = "average")
  n <- base::tapply(data$AVAL, data$TRTP, base::length)
  n1 <- n[["A"]]
  n0 <- n[["P"]]

  d <- base::data.frame(R1 = A, R2 = base::c(B$A, B$P), TRTP = base::c(base::rep("A", n1), base::rep("P", n0)))
  d$R <- d$R1 - d$R2
  d$R0 <- base::ifelse(d$TRTP == "A", d$R/n0, d$R/n1)


  WP0 <- base::tapply(d$R0, d$TRTP, base::mean)
  VAR <- base::tapply(d$R0, d$TRTP, function(x) (base::length(x)-1)*stats::var(x)/base::length(x))
  SE_WP <- base::sqrt(base::sum(VAR/n))

  WP = WP0[["A"]]
  WO = WP/(1 - WP)
  SE <- SE_WP/(WP*(1-WP))
  Ca <- stats::qnorm(1 - alpha/2)
  LCL <- WO*base::exp(-Ca*SE)
  UCL <- WO*base::exp(Ca*SE)

  threshold <- base::abs(WP - WPnull)/SE_WP
  P <- 2*(1 - stats::pnorm(threshold))

  out <- base::data.frame(WO = WO, LCL = LCL, UCL = UCL, SE = SE, WOnull = WOnull, alpha = alpha, Pvalue = P)

  return(out)
}
