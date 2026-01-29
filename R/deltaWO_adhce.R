#' Win odds calculation based on a threshold for `adhce` objects
#'
#' @param x an `adhce` object.
#' @param delta a numeric threshold. 
#' @param ref the reference treatment group.
#' @param alpha  significance level. The default is 0.05.
#' @param WOnull the null hypothesis. The default is 1.
#' @param ...  additional parameters.
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
#' * ref the reference group.
#' * delta the threshold.
#' @export
#' @md
#' @seealso [hce::deltaWO()], [hce::calcWO()].
#' @examples
#' # Example using the kidney dataset
#' dat <- as_hce(KHCE)
#' calcWO(dat, ref = "P")
#' ## The exact same result
#' deltaWO(dat, delta = 0, ref = "P")
#' ## A large threshold
#' deltaWO(dat, delta = 10, ref = "P")
#' 
deltaWO.adhce <- function (x, delta, ref = unique(x$TRTP)[1], alpha = 0.05, WOnull = 1, ...) 
{
  # Even if the object has the class `adhce`, still recreate it to catch all issues.
  dat <- as_hce(x)
  delta <- delta[1]
  
  base::stopifnot("The threshold `delta` must be nonnegative." = delta >= 0)

  alpha <- alpha[1]
  WOnull <- WOnull[1]
  WPnull <- WOnull/(WOnull + 1)
  ref <- ref[1]
  SEV <- base::levels(base::factor(dat$GROUP))
  LAST <- SEV[base::length(SEV)]
  
  dat$AVAL01 <- dat$AVAL02 <- dat$AVAL0
  dat$AVAL01[dat$GROUP == LAST & dat$TRTP == ref] <- dat$AVAL0[dat$GROUP == LAST & dat$TRTP == ref] - delta
  dat$AVAL02[dat$GROUP == LAST & dat$TRTP == ref] <- dat$AVAL0[dat$GROUP == LAST & dat$TRTP == ref] + delta
  mcont1 <- base::min(dat[dat$GROUP == LAST, "AVAL01"])
  mcont2 <- base::min(dat[dat$GROUP == LAST, "AVAL02"])
  dat$AVAL1 <- base::ifelse(dat$GROUP != LAST, dat$AVAL01 + dat$GROUPN, 
                      dat$AVAL01 - mcont1 + 1 + dat$GROUPN)
  dat$AVAL2 <- base::ifelse(dat$GROUP != LAST, dat$AVAL02 + dat$GROUPN, 
                      dat$AVAL02 - mcont2 + 1 + dat$GROUPN)
  
  dat1 <- IWP(data = dat, AVAL = "AVAL1", TRTP = "TRTP", ref = ref)
  ## Brings back the original treatment assignment given that IWP() converts the treatment to "A" and "P"
  dat1_ <- merge(dat1[names(dat) != "TRTP"], dat[, c("ID", "TRTP")], all.x = TRUE, by = "ID")
  dat2 <- IWP(data = dat1_, AVAL = "AVAL2", TRTP = "TRTP", ref = ref)
  dat2$AVAL_ <- (dat2$AVAL1_ + dat2$AVAL2_)/2
  
  n <- base::tapply(dat2$AVAL1_, dat2$TRTP, base::length)
  SE_WP <- sqrt(sum(tapply(dat2$AVAL_, dat2$TRTP, function(x) (length(x) - 1)*stats::var(x)/base::length(x))/n))
  WP1 <- base::tapply(dat2$AVAL1_, dat2$TRTP, base::mean)
  WP2 <- base::tapply(dat2$AVAL2_, dat2$TRTP, base::mean)
  ## Will always be "A" and "P" because of IWP()
  WP <- (WP1["A"] + WP2["A"])/2
  WO <- WP/(1 - WP)
  
  SD_WP <- SE_WP*base::sqrt(sum(n))
  SE <- SE_WP/(WP*(1 - WP))
  Ca <- stats::qnorm(1 - alpha/2)
  LCL <- WO*base::exp(- Ca*SE)
  UCL <- WO*base::exp(Ca*SE)
  
  LCL_WP <- WP - Ca*SE_WP
  UCL_WP <- WP + Ca*SE_WP
  threshold <- base::abs(WP - WPnull)/SE_WP
  P <- 2*(1 - stats::pnorm(threshold))
  
  out <- base::data.frame(WO = WO, LCL = LCL, UCL = UCL, SE = SE, WOnull = WOnull, alpha = alpha, 
                          Pvalue = P, WP = WP, LCL_WP  = LCL_WP, UCL_WP = UCL_WP, SE_WP = SE_WP, 
                          SD_WP = SD_WP, N = sum(n), ref = ref, delta = delta)
  return(out)
  
}
