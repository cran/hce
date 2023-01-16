#' Proportion of wins/losses/ties given the win odds and the win ratio
#'
#' @param WO win odds.
#' @param WR win ratio.
#' @param Overall number of comparisons, the sample size of 
#' the active treatment multiplied by the sample size of the placebo. 
#' The default is 1, hence gives the proportion. 
#' @details 
#' ![](propWINS.png "Calculation formula")
#' @return a data frame with a number (or proportion if `Overall = 1`) of wins/losses/ties.
#' @export
#'
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
propWINS <- function(WO, WR, Overall = 1){
  WO <- WO[1]
  WR <- WR[1]
  Overall <- Overall[1]
  
  WP <- WO/(WO + 1)
  L <- Overall*(2*WP - 1)/(WR - 1)
  W <- WR*L
  TIE <- Overall - W - L
  data.frame(WIN = W, LOSS = L, TIE = TIE, TOTAL = Overall)
}