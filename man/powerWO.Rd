% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/powerWO.R
\name{powerWO}
\alias{powerWO}
\title{Power calculation for the win odds test (no ties)}
\usage{
powerWO(
  N,
  WO,
  SD = NULL,
  k = 0.5,
  alpha = 0.05,
  WOnull = 1,
  alternative = c("shift", "max", "ordered")
)
}
\arguments{
\item{N}{a numeric vector of sample size values.}

\item{WO}{the given win odds for the alternative hypothesis. A numeric vector of length 1.}

\item{SD}{assumed standard deviation of the win proportion. By default uses the conservative SD. A numeric vector of length 1.}

\item{k}{proportion of active group in the overall sample size. Default is 0.5 (balanced randomization). A numeric vector of length 1.}

\item{alpha}{the significance level for the 2-sided test. Default is 0.05. A numeric vector of length 1.}

\item{WOnull}{the win odds value of the null hypothesis (default is 1). A numeric vector of length 1.}

\item{alternative}{a character string specifying the class of the alternative hypothesis, must be one of \code{"shift"} (default), \code{"max"} or \code{"ordered"}. You can specify just the initial letter.}
}
\value{
a data frame containing the calculated power with input values.
}
\description{
Power calculation for the win odds test (no ties)
}
\details{
\code{alternative = "max"} refers to the maximum variance of the win proportion across all possible
alternatives. The maximum variance equals \code{WP*(1 - WP)/k} where the win probability is calculated as \verb{WP = WO/(WO + 1).}
\code{alternative = "shift"} specifies the variance across alternatives from a shifted family of distributions (Wilcoxon test). The variance formula, as suggested by Noether, is calculated based on the null hypothesis as follows \verb{1/(12*k*(1 - k)).}
\code{alternative = "ordered"} specifies the variance across alternatives from stochastically ordered distributions which include shifted distributions.
}
\examples{
# Example 1- Use the default standard deviation
powerWO(N = 1000, WO = 1.2)
powerWO(N = seq(500, 1500, 100), WO = 1.2)
# Example 2 - Use data-driven win odds and standard deviation from the COVID19 dataset
res <- calcWO(x = COVID19, AVAL = "GROUP", TRTP = "TRTP", ref = "Placebo")
print(res)
powerWO(N = 500, WO = res$WO, SD = res$SD_WP)
powerWO(N = 500, WO = res$WO) # power with the default standard deviation for the win proportion.
# Example 3 - Non-balanced 3:1 randomization
powerWO(N = 1000, WO = 1.2, k = 0.75)
# Example 4 - Comparison of different alternatives
powerWO(N = 1000, WO = 1.2, alternative = "m")
powerWO(N = 1000, WO = 1.2, alternative = "s")
powerWO(N = 1000, WO = 1.2, alternative = "o")
}
\references{
\itemize{
\item All formulas were presented in
\cr \cr Bamber D (1975) "The area above the ordinal dominance graph and the area below the receiver operating characteristic graph." Journal of Mathematical Psychology 12.4: 387-415. \url{doi:10.1016/0022-2496(75)90001-2}.
\item Noether's formula for shifted alternatives
\cr \cr Noether GE (1987) "Sample size determination for some common nonparametric tests." Journal of the American Statistical Association 82.398: 645-7. \url{doi:10.1080/01621459.1987.10478478}.
\item For shift alternatives see also
\cr \cr Gasparyan SB et al. (2021) "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6: 765-787. \url{doi:10.1080/10543406.2021.1968893}.
}
}
\seealso{
\code{\link[=sizeWO]{sizeWO()}}, \code{\link[=minWO]{minWO()}} for WO sample size or minimum detectable WO calculation.
}
