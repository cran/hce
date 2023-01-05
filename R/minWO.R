

#' Minimum detectable or WO for alternative hypothesis for given power (no ties)
#'
#' @param N a numeric vector of sample size values (two arms combined).
#' @param power the given power. The default is 0.5 corresponding to the minimum detectable win odds. A numeric vector of length 1.
#' @param SD assumed standard deviation of the win proportion. By default uses the conservative SD. A numeric vector of length 1.
#' @param alpha the significance level for the 2-sided test. Default is 0.05. A numeric vector of length 1.
#' @param WOnull the win odds value of the null hypothesis (default is 1). A numeric vector of length 1.
#' @param digits precision to use for reporting calculated win odds.
#'
#' @return a data frame containing the calculated WO with input values.
#' @export
#' @md
#' @seealso [hce::powerWO()], [hce::sizeWO()]  for WO power and sample size calculation.
#' @references Gasparyan, Samvel B., et al. "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6 (2021): 765-787. <doi:10.1080/10543406.2021.1968893>
#' @examples
#' minWO(N = 100, digits = 5)
#'minWO(N = 1200, power = 0.9)
minWO <- function(N, power = 0.5, SD = 1/sqrt(3), alpha = 0.05, WOnull = 1, digits = 2){
  digits <- digits[1]
  power <- power[1]
  SD <- SD[1]
  alpha <- alpha[1]
  WOnull <- WOnull[1]
  WPnull <- WOnull/(WOnull + 1)
  SE <- SD/base::sqrt(N)
  C <- stats::qnorm(1-alpha/2)

  WP <- WPnull + SE*(C - stats::qnorm(1 - power))
  WO <- WP/(1 - WP)
  d <- base::data.frame(WO = base::round(WO, digits), WP = WP, SampleSize = N, power = power, WPnull = WPnull, WOnull = WOnull, alpha = alpha, SD = SD)


    l <- base::list(result = d, value = WO, input = N, input_name = "Sample Size", value_name = "Win Odds")
    base::class(l) <- c("hce_results")
  l
}
