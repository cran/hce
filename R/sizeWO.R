#' Sample size calculation for the win odds test (no ties)
#'
#' @param WO a numeric vector of win odds values.
#' @param power the given power. A numeric vector of length 1.
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
#' @return a data frame containing the sample size with input values.
#' @export
#' @md
#' @seealso [hce::powerWO()], [hce::minWO()] for WO power or minimum detectable WO calculation.
#' @references 
#' * All formulas were presented in 
#' \cr \cr Bamber D (1975) "The area above the ordinal dominance graph and the area below the receiver operating characteristic graph." Journal of Mathematical Psychology 12.4: 387-415. <doi:10.1016/0022-2496(75)90001-2>.
#' * Noether's formula for shifted alternatives 
#' \cr \cr Noether GE (1987) "Sample size determination for some common nonparametric tests." Journal of the American Statistical Association 82.398: 645-7. <doi:10.1080/01621459.1987.10478478>.
#' * For shift alternatives see also 
#' \cr \cr Gasparyan SB et al. (2021) "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6: 765-787. <doi:10.1080/10543406.2021.1968893>.
#' @examples
#' sizeWO(WO = 1.25, power = 0.9)
#' sizeWO(WO = 1.25, power = 0.9, k = 0.75)
#' sizeWO(WO = seq(1.05, 1.5, 0.05), power = 0.9)
#' # Comparison of different alternatives
#' x <- seq(1.05, 5, 0.05)
#' N1 <- sizeWO(WO = x, power = 0.9, alternative = "m")$SampleSize
#' N2 <- sizeWO(WO = x, power = 0.9, alternative = "o")$SampleSize
#' N3 <- sizeWO(WO = x, power = 0.9, alternative = "s")$SampleSize
#' d <- data.frame(WO = x, N_m = N1, N_o = N2, N_s = N3)
#' ## Check the power for the ordered alternative
#' check <- c()
#' for(i in seq_along(x)){
#' check[i] <- powerWO(N = d[i, "N_o"], WO = d[i, "WO"], alternative = "o")$power
#' }
#' d$power_check_o <- check
#' print(d)

sizeWO <- function(WO, power, SD = NULL, k = 0.5, alpha = 0.05, WOnull = 1, alternative = c("shift", "max", "ordered")){
  power <- power[1]
  SD <- SD[1]
  alpha <- alpha[1]
  WOnull <- WOnull[1]
  WP <- WO/(WO + 1)
  k <- k[1]
  WPnull <- WOnull/(WOnull + 1)
  if(k < 0 | k > 1) stop("k should be between 0 and 1.")
  if (any(WO <= 1)) 
    stop("WO should be > 1 for superiority.")
  
  C <- stats::qnorm(1 - alpha/2)
  k <- min(k, 1- k)
  alternative <- match.arg(alternative)
  VAR <- switch(alternative,
                shift = 1/(12 * k * (1 - k)),
                max = WP*(1 - WP)/k ,
                ordered = 1/(k*(1 - k))*(
                  2*k*WP*(1 - WP) - 
                    (1 - 2*k)*(1 - WP)^2 + 
                    1/3*(1 - 3*k)*( 1 - (2*WP - 1)^1.5)
                ))
  if (is.null(SD)){
    SD <- sqrt(VAR)
  }
  SS <- SD^2/(WP - WPnull)^2*(C - stats::qnorm(1 - power))^2
  d <- base::data.frame(WO = WO, WP = WP, power = power, SampleSize = base::ceiling(SS), SD = SD, alpha = alpha, WPnull = WPnull, WOnull = WOnull, k = k)
  l <- base::list(result = d, value = base::ceiling(SS), input = WO, input_name = "Win Odds", value_name = "Sample Size")
  attr(d, "res") <- l
  base::class(d) <- c("hce_results")
  d
}

