#' Sample size calculation for the win odds test (no ties)
#'
#' @param WO a numeric vector of win odds values.
#' @param power the given power. A numeric vector of length 1.
#' @param SD assumed standard deviation of the win proportion. By default uses the conservative SD. A numeric vector of length 1.
#' @param alpha the significance level for the 2-sided test. Default is 0.05. A numeric vector of length 1.
#' @param WOnull the win odds value of the null hypothesis (default is 1). A numeric vector of length 1.
#'
#' @return a data frame containing the sample size with input values.
#' @export
#' @md
#' @seealso [hce::powerWO()], [hce::minWO()] for WO power or minimum detectable WO calculation.
#' @references Gasparyan, Samvel B., et al. "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6 (2021): 765-787. <doi:10.1080/10543406.2021.1968893>
#' @examples
#' sizeWO(WO = 1.25, power = 0.9)
#' sizeWO(WO = seq(1.05, 1.5, 0.05), power = 0.9)



sizeWO <- function(WO, power, SD = 1/sqrt(3), alpha = 0.05, WOnull = 1){
  power <- power[1]
  SD <- SD[1]
  alpha <- alpha[1]
  WOnull <- WOnull[1]

  WP <- WO/(WO + 1)
  WPnull <- WOnull/(WOnull + 1)
  C <- stats::qnorm(1 - alpha/2)

    SS <- SD^2/(WP - WPnull)^2*(C - stats::qnorm(1 - power))^2

  d <- base::data.frame(WO = WO, WP = WP, power = power, SampleSize = base::ceiling(SS), SD = SD, alpha = alpha, WPnull = WPnull, WOnull = WOnull)

  l <- base::list(result = d, value = base::ceiling(SS), input = WO, input_name = "Win Odds", value_name = "Sample Size")
  base::class(l) <- c("hce_results")
  l

}

