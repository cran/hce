
#' Power calculation for the win odds test (no ties)
#'
#' @param N a numeric vector of sample size values.
#' @param WO the given win odds for the alternative hypothesis. A numeric vector of length 1.
#' @param SD assumed standard deviation of the win proportion. By default uses the conservative SD. A numeric vector of length 1.
#' @param alpha the significance level for the 2-sided test. Default is 0.05. A numeric vector of length 1.
#' @param WOnull the win odds value of the null hypothesis (default is 1). A numeric vector of length 1.
#'
#' @return a data frame containing the calculated power with input values.
#' @export
#' @md
#' @seealso [hce::sizeWO()], [hce::minWO()] for WO sample size or minimum detectable WO calculation.
#' @references Gasparyan, Samvel B., et al. "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6 (2021): 765-787. <doi:10.1080/10543406.2021.1968893>.
#' @examples
#' # Example 1- Use the default standard deviation
#' powerWO(N = 1000, WO = 1.2)
#' powerWO(N = seq(500, 1500, 100), WO = 1.2)
#' # Example 2 - Use data-driven win odds and standard deviation from the COVID19 dataset
#' res <- calcWO(x = COVID19, AVAL = "GROUP", TRTP = "TRTP", ref = "P")
#' print(res)
#' powerWO(N = 500, WO = res$WO, SD = res$SD_WP)
#' powerWO(N = 500, WO = res$WO) # power with the default standard deviation for the win proportion.



powerWO <- function(N, WO, SD = 1/sqrt(3), alpha = 0.05, WOnull = 1){
  WO <- WO[1]
  SD <- SD[1]
  alpha <- alpha[1]
  WOnull <- WOnull[1]

  WP <- WO/(WO + 1)
  WPnull <- WOnull/(WOnull + 1)
  SE <- SD/base::sqrt(N)
  C <- stats::qnorm(1-alpha/2)

  power <- stats::pnorm(-C + (WP - 0.5)/SE) + stats::pnorm( -C - (WP - WPnull)/SE)
  d <- base::data.frame(power = power, SampleSize = N, WO = WO, WP = WP, WPnull = WPnull, WOnull = WOnull, alpha = alpha, SD = SD)

  l <- base::list( result = d, value = power, input = N, input_name = "Sample Size", value_name = "Power")
  base::class(l) <- c("hce_results")
  l
}

