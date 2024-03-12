#' Proportion of wins/losses/ties given the win odds and the win ratio
#'
#' @param WO win odds.
#' @param WR win ratio.
#' @param Overall number of comparisons, the sample size of 
#' the active treatment multiplied by the sample size of the placebo. 
#' The default is 1, hence gives the proportion. 
#' @param alpha significance level for the win ratio confidence interval. The default is `NULL` hence the confidence interval is not produced.
#' @param N the combined sample size of two treatment groups. The default is `NULL`. If `alpha` is specified then either `N` should be specified or `Overall > 1`. 
#' For given `Overall`, the pooled sample size is calculated as `N = 2*sqrt(Overall)`.
#' @details 
#' ![](propWINS.png "Calculation formula")
#' @return a data frame with a number (or proportion if `Overall = 1`) of wins/losses/ties. If `alpha` is specified returns also `WR` confidence interval.
#' @export
#' @md
#' @references 
#' * For the relationship between win odds and win ratio see 
#' \cr \cr Gasparyan SB et al. "Hierarchical Composite Endpoints in COVID-19: The DARE-19 Trial". Case Studies in Innovative Clinical Trials, Chapter 7 (2023): 95-148. Chapman and Hall/CRC. <doi:10.1201/9781003288640-7>.
#' * The win ratio CI uses the standard error presented in
#' \cr \cr Yu RX, Ganju J. (2022) "Sample size formula for a win ratio endpoint." Statistics in Medicine 41.6: 950-63. <doi:10.1002/sim.9297>.
#' @examples
#' # Example 1
#' propWINS(WR = 2, WO = 1.5)
#' # Example 2 - Back-calculation 
#' COVID19HCE <- hce(GROUP = COVID19$GROUP, TRTP = COVID19$TRTP)
#' res <- calcWINS(COVID19HCE)
#' WR <- res$WR1$WR
#' WO <- res$WO$WO
#' Overall <- res$summary$TOTAL
#' propWINS(WR = WR, WO = WO, Overall = Overall)
#' ## Verify 
#' res$summary
#' # Example 3 - Confidence interval
#'  propWINS(WR = 1.4, WO = 1.3, alpha = 0.05, Overall = 2500)
#'  propWINS(WR = 2, WO = 1.5, alpha = 0.01, N = 500)
propWINS <- function(WO, WR, Overall = 1, alpha = NULL, N = NULL){
  WO <- WO[1]
  WR <- WR[1]
  Overall <- Overall[1]
  alpha <- alpha[1]
  
  WP <- WO/(WO + 1)
 
  
  if(is.null(alpha)) {
    L <- Overall*(2*WP - 1)/(WR - 1)
    L <- round(L)
    W <- round(WR*L)
    TIE <- Overall - W - L
    d <- data.frame(WIN = W, LOSS = L, TIE = TIE, TOTAL = Overall, WR = WR, WO = WO)
    return(d)
  } else {
    if(is.null(N) & Overall == 1)
      stop("Either Overall should be > 1 or N should be specified.")
    
    if(!is.null(N) & Overall > 1)
      stop("Either Overall and N cannot both be specified.")
    
    if(Overall > 1) 
      N <- 2*sqrt(Overall)
    
    if(Overall == 1 & !is.null(N)) 
      Overall <- (N/2)^2
    
    L <- Overall*(2*WP - 1)/(WR - 1)
    L <- round(L)
    W <- round(WR*L)
    TIE <- Overall - W - L
    d <- data.frame(WIN = W, LOSS = L, TIE = TIE, TOTAL = Overall, WR = WR, WO = WO)
    
    
    k <- 0.5
    Ca <- stats::qnorm(1 - alpha/2)
    VAR <- 1/N*(4/(3*k*(1 - k)))*(1 + TIE/Overall)/(1 - TIE/Overall)
    CL <- WR*exp(Ca*sqrt(VAR)*c(- 1, 1))
    d$WR_LCL <- CL[1]
    d$WR_UCL <- CL[2]
    d$alpha <- alpha
    d$N <- N
    return(d)
  }
  
}