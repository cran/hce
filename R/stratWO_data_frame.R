#' Stratified win odds with adjustment
#'
#' @param x a data frame containing subject-level data.
#' @param AVAL variable in the data with ordinal analysis values.
#' @param TRTP the treatment variable in the data.
#' @param STRATA a character variable for stratification.
#' @param ref the reference treatment group.
#' @param COVAR a numeric covariate.
#' @param alpha the reference treatment group.
#' @param WOnull the null hypothesis. The default is 1.
#' @param ... additional parameters.
#' @returns a data frame containing the following columns:
#' * WO stratified (or adjusted/stratified) win odds.
#' * LCL lower confidence limit for adjusted (or adjusted/stratified) WO.
#' * UCL upper confidence limit for adjusted (or adjusted/stratified) WO.
#' * SE standard error of the adjusted (or adjusted/stratified) win odds.
#' * WOnull win odds of the null hypothesis (specified in the `WOnull` argument).
#' * alpha two-sided significance level for calculating the confidence interval (specified in the `alpha` argument).
#' * Pvalue p-value associated with testing the null hypothesis.
#' * WP adjusted (or adjusted/stratified) win probability.
#' * SE_WP standard error for the adjusted (or adjusted/stratified) win probability.
#' * SD_WP standard deviation of the adjusted (or adjusted/stratified) win probability.
#' * N total number of patients in the analysis.
#' * Type "STRATIFIED" or "STRATIFIED/ADJUSTED" depending on whether `COVAR` is specified.
#' @export
#' @md
#' @seealso [hce::stratWO()].
#' @references Gasparyan SB et al. (2021) "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2: 580-611. <doi:10.1177/0962280220942558>
#' @examples
#' # Stratified win odds
#' res <- stratWO(x = KHCE, AVAL = "AVAL", TRTP = "TRTP", 
#' STRATA = "STRATAN", ref = "P")
#' res
#' ## Compare with the win odds in each stratum separately
#' lapply(split(KHCE, KHCE$STRATAN), calcWO, AVAL = "AVAL", TRTP = "TRTP", ref = "P")
#' # Stratified and adjusted win odds
#' res <- stratWO(x = KHCE, AVAL = "AVAL", COVAR = "EGFRBL", 
#' TRTP = "TRTP", STRATA = "STRATAN", ref = "P")
#' res
stratWO.data.frame <- function(x, AVAL, TRTP, STRATA, ref, COVAR = NULL, alpha = 0.05, WOnull = 1, ...){
  data <- as.data.frame(x)
  alpha <- alpha[1]
  ref <- ref[1]
  WOnull <- WOnull[1]
  AVAL <- AVAL[1]
  STRATA <- STRATA[1]
  COVAR <- COVAR[1]
  WPnull <- WOnull/(WOnull + 1)
  Ca <- stats::qnorm(1 - alpha/2)
  

  if(! AVAL %in% base::names(data))
    stop(paste0("The variable ", AVAL, " is not in the dataset."))
  if(! TRTP %in% base::names(data))
    stop(paste0("The variable ", TRTP, " is not in the dataset."))
  if(! STRATA %in% base::names(data))
    stop(paste0("The variable ", STRATA, " is not in the dataset."))
  
      
  data$AVAL <- data[, base::names(data) == AVAL]
  data$TRTP <- data[, base::names(data) == TRTP]
  data$STRATA <- data[, base::names(data) == STRATA]
  
  if (length(unique(data$TRTP)) != 2) 
    stop("The dataset should contain two treatment groups.")
  if (!ref %in% unique(data$TRTP)) 
    stop("Choose the reference from the values in TRTP.")
  data$TRTP <- base::ifelse(data$TRTP == ref, "P", "A")
  
  
  
  l <- split(data, data$STRATA)
  
  if(!is.null(COVAR)){
    data$COVAR <- data[, base::names(data) == COVAR]
    
    if(! COVAR %in% base::names(data))
      stop(paste0("The variable ", COVAR, " is not in the dataset."))
    if(!is.numeric(data$COVAR)) 
      stop("COVAR should be numeric.") 
    
    ll2 <- lapply(l, regWO, AVAL = "AVAL", TRTP = "TRTP", ref = "P", COVAR = COVAR, alpha = alpha, WOnull = WOnull)
    STR2 <- do.call(rbind, ll2)
    STR2$STRATA <- row.names(STR2)
    row.names(STR2) <- NULL
    
    STR_tab <- table(data$STRATA, data$TRTP)
    STR_frame <- data.frame(STRATA = row.names(STR_tab), TRT_A = STR_tab[, "A"], TRT_P = STR_tab[, "P"])
    row.names(STR_frame) <- NULL
    STR0 <- merge(STR2, STR_frame, by = "STRATA")
    STR0$omega <- STR0$TRT_A*STR0$TRT_P/STR0$N
    STR0$omega0 <- sum(STR0$omega)
    STR0$weight <- STR0$omega/STR0$omega0
    
    WP_str <- sum(STR0$WP * STR0$weight)
    SE_WP_str <- sqrt(sum((STR0$SE_WP * STR0$weight)^2))
    adjust <- sum(STR0$weight * STR0$COVAR_MEAN_DIFF)*sum(STR0$weight^2 * STR0$COVAR_COV)/sum(STR0$weight^2 * STR0$COVAR_VAR)
    adjust_VAR <- sum(STR0$weight^2 * STR0$COVAR_COV)^2/sum(STR0$weight^2 * STR0$COVAR_VAR)
    WP <- WP_str - adjust
    SE_WP <- sqrt(SE_WP_str^2 - adjust_VAR)
   
    threshold <- abs(WP - WPnull)/SE_WP
    P <- 2 * (1 - stats::pnorm(threshold))
    WO <- WP/(1 - WP) 
    SE <-  SE_WP/(WP*(1 - WP))
    LCL <- WO*exp(- Ca *SE)
    UCL <- WO*exp(Ca *SE)
    
    res <- data.frame( WO = WO, LCL = LCL, UCL = UCL, SE = SE, WOnull = WOnull, alpha = alpha, Pvalue = P, 
                       WP = WP, SE_WP = SE_WP, SD_WP = SE_WP*sqrt(sum(STR0$N)), N = sum(STR0$N))
    res$Type <- "STRATIFIED/ADJUSTED"
    
  } else{
    ll <- lapply(l, calcWO, AVAL = "AVAL", TRTP = "TRTP", ref = "P", alpha = alpha, WOnull = WOnull)
    
    STR <- do.call(rbind, ll)
    STR$STRATA <- row.names(STR)
    row.names(STR) <- NULL
    
    STR_tab <- table(data$STRATA, data$TRTP)
    STR_frame <- data.frame(STRATA = row.names(STR_tab), TRT_A = STR_tab[, "A"], TRT_P = STR_tab[, "P"])
    row.names(STR_frame) <- NULL
    STR0 <- merge(STR, STR_frame, by = "STRATA")
    STR0$omega <- STR0$TRT_A*STR0$TRT_P/STR0$N
    STR0$omega0 <- sum(STR0$omega)
    STR0$weight <- STR0$omega/STR0$omega0
    
    WP_str <- sum(STR0$WP * STR0$weight)
    SE_WP_str <- sqrt(sum((STR0$SE_WP * STR0$weight)^2))
    threshold <- abs(WP_str - WPnull)/SE_WP_str
    P <- 2 * (1 - stats::pnorm(threshold))
    WO_str <- WP_str/(1 - WP_str) 
    SE_str <-  SE_WP_str/(WP_str*(1 - WP_str))
    LCL_str <- WO_str*exp(- Ca *SE_str)
    UCL_str <- WO_str*exp(Ca*SE_str)
    
    res <- data.frame( WO = WO_str, LCL = LCL_str, UCL = UCL_str, SE = SE_str, WOnull = WOnull, alpha = alpha, Pvalue = P, 
                       WP = WP_str, SE_WP = SE_WP_str, SD_WP = SE_WP_str*sqrt(sum(STR0$N)), N = sum(STR0$N))
    res$Type <- "STRATIFIED"
  }
  
  return(res)
  
}


