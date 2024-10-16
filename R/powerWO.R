
#' Power calculation for the win odds test (no ties)
#'
#' @param N a numeric vector of sample size values.
#' @param WO the given win odds for the alternative hypothesis. A numeric vector of length 1.
#' @param SD assumed standard deviation of the win proportion. By default uses the conservative SD. A numeric vector of length 1.
#' @param k proportion of active group in the overall sample size. Default is 0.5 (balanced randomization). A numeric vector of length 1.
#' @param alpha the significance level for the 2-sided test. Default is 0.05. A numeric vector of length 1.
#' @param WOnull the win odds value of the null hypothesis (default is 1). A numeric vector of length 1.
#' @param alternative a character string specifying the class of the alternative hypothesis, must be one of `"shift"` (default), `"max"` or `"ordered"`. You can specify just the initial letter.
#'
#' @details
#' `alternative = "max"` refers to the maximum variance of the win proportion across all possible
#' alternatives. The maximum variance equals `WP*(1 - WP)/k` where the win probability is calculated as `WP = WO/(WO + 1).`
#' `alternative = "shift"` specifies the variance across alternatives from a shifted family of distributions (Wilcoxon test). The variance formula, as suggested by Noether, is calculated based on the null hypothesis as follows `1/(12*k*(1 - k)).` 
#' `alternative = "ordered"` specifies the variance across alternatives from stochastically ordered distributions which include shifted distributions. 
#' 
#' @return a data frame containing the calculated power with input values.
#' @export
#' @md
#' @seealso [hce::sizeWO()], [hce::minWO()] for WO sample size or minimum detectable WO calculation.
#' @references 
#' * All formulas were presented in 
#' \cr \cr Bamber D (1975) "The area above the ordinal dominance graph and the area below the receiver operating characteristic graph." Journal of Mathematical Psychology 12.4: 387-415. <doi:10.1016/0022-2496(75)90001-2>.
#' * Noether's formula for shifted alternatives 
#' \cr \cr Noether GE (1987) "Sample size determination for some common nonparametric tests." Journal of the American Statistical Association 82.398: 645-7. <doi:10.1080/01621459.1987.10478478>.
#' * For shift alternatives see also 
#' \cr \cr Gasparyan SB et al. (2021) "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6: 765-787. <doi:10.1080/10543406.2021.1968893>.
#' @examples
#' # Example 1- Use the default standard deviation
#' powerWO(N = 1000, WO = 1.2)
#' powerWO(N = seq(500, 1500, 100), WO = 1.2)
#' # Example 2 - Use data-driven win odds and standard deviation from the COVID19 dataset
#' res <- calcWO(x = COVID19, AVAL = "GROUP", TRTP = "TRTP", ref = "Placebo")
#' print(res)
#' powerWO(N = 500, WO = res$WO, SD = res$SD_WP)
#' powerWO(N = 500, WO = res$WO) # power with the default standard deviation for the win proportion.
#' # Example 3 - Non-balanced 3:1 randomization
#' powerWO(N = 1000, WO = 1.2, k = 0.75)
#' # Example 4 - Comparison of different alternatives
#' powerWO(N = 1000, WO = 1.2, alternative = "m")
#' powerWO(N = 1000, WO = 1.2, alternative = "s")
#' powerWO(N = 1000, WO = 1.2, alternative = "o")
powerWO <- function(N, WO, SD = NULL, k = 0.5, alpha = 0.05, WOnull = 1, alternative = c("shift", "max", "ordered")){
  WO <- WO[1]
  SD <- SD[1]
  alpha <- alpha[1]
  WOnull <- WOnull[1]
  k <- k[1]
  if (k < 0 | k > 1) 
    stop("k should be between 0 and 1.")
  if (WO <= 1) 
    stop("WO should be > 1 for superiority.")
  k <- min(k, 1- k)
  alternative <- match.arg(alternative)
  WP <- WO/(WO + 1)
  WPnull <- WOnull/(WOnull + 1)
  SD_method <- "Input"
  VAR <- switch(alternative,
                shift = 1/(12 * k * (1 - k)),
                max = WP*(1 - WP)/k ,
                ordered = 1/(k*(1 - k)*N)*(
                  (2*k*N - 1)*WP*(1 - WP) - 
                    (1 - 2*k)*N*(1 - WP)^2 + 
                    1/3*((1 - 3*k)*N + 1)*( 1 - (2*WP - 1)^1.5)
                ))
      if (is.null(SD)){
      SD <- sqrt(VAR)
      SD_method <- alternative
  }
  SE <- SD/base::sqrt(N)
  C <- stats::qnorm(1 - alpha/2)
  power <- stats::pnorm(-C + (WP - WPnull)/SE) + stats::pnorm(-C - 
                                                                (WP - WPnull)/SE)
  d <- base::data.frame(power = power, SampleSize = N, WO = WO, 
                        WP = WP, WPnull = WPnull, WOnull = WOnull, alpha = alpha, 
                        SD = SD, SD_method = SD_method, k = k)
  l <- base::list(result = d, value = power, input = N, input_name = "Sample Size", 
                  value_name = "Power")
  attr(d, "res") <- l
  base::class(d) <- c("hce_results")
  d
}

