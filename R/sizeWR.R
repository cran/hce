#' Sample size calculation for the win ratio test (with WR = 1 null hypothesis)
#'
#' @param WR a numeric vector of win odds values.
#' @param power the given power. A numeric vector of length 1.
#' @param WO win odds. Should be specified only if `Pties` is not specified. A numeric vector of length 1.
#' @param Pties probability of ties. A numeric vector of length 1.
#' @param k proportion of active group in the overall sample size. Default is 0.5 (balanced randomization). A numeric vector of length 1.
#' @param alpha the significance level for the 2-sided test. Default is 0.05. A numeric vector of length 1.
#'
#' @return a data frame containing the sample size with input values.
#' @export
#' @md
#' @seealso [hce::sizeWO()] for WO sample size calculation.
#' @references Yu RX, Ganju J. (2022) "Sample size formula for a win ratio endpoint." Statistics in Medicine, 41.6: 950-63. <doi:10.1002/sim.9297>.
#' @examples
#' sizeWR(WR = 1.36, Pties = 0.064, power = 0.9)
#' sizeWR(WR = 1.36, WO = 1.333, power = 0.9)

sizeWR <- function(WR, power, WO = NULL, Pties = NULL, k = 0.5, alpha = 0.05){
  alpha <- alpha[1]
  power <- power[1]
  k <- k[1]
  Ca <- stats::qnorm(1 - alpha/2)
  Cpow <- stats::qnorm(power)
  if(k < 0 | k > 1) stop("k should be between 0 and 1.")
  if(!is.null(WO) & !is.null(Pties)) stop("Either win odds or the probability of ties `Pties` should be specified.")
  if(is.null(Pties)) {
    WP <- WO/(WO + 1)
    Pties <- 1 - (WR + 1)*(2 * WP - 1)/(WR - 1)
  }
  coef <- 4*(1 + Pties)/(3*k*(1 - k)*(1 - Pties))
  N <- coef * (Ca + Cpow)^2 / log(WR)^2
  base::data.frame(WR = WR, Pties = Pties, power = power, 
                   SampleSize = base::ceiling(N), 
                   alpha = alpha)
}