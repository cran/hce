% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcWINS_formula.R
\name{calcWINS.formula}
\alias{calcWINS.formula}
\title{Win statistics calculation using formula syntax}
\usage{
\method{calcWINS}{formula}(x, data, ...)
}
\arguments{
\item{x}{an object of class formula.}

\item{data}{a data frame.}

\item{...}{additional parameters.}
}
\value{
a list containing win statistics and their confidence intervals. It contains the following named data frames:
\itemize{
\item summary a data frame containing number of wins, losses, and ties of the active treatment group and the overall number of comparisons.
\item WP a data frame containing the win probability and its confidence interval.
\item NetBenefit a data frame containing the net benefit and its confidence interval. This is just a \verb{2x-1} transformation of WP and its CI.
\item WO a data frame containing the win odds and its confidence interval.
\item WR1 a data frame containing the win ratio and its confidence interval, using the transformed standard error of the \code{gamma} statistic.
\item WR2 a data frame containing the win ratio and its confidence interval, using the standard error calculated using \code{Pties}.
\item gamma a data frame containing Goodman Kruskal's \code{gamma} and its confidence interval.
\item SE a data frame containing standard errors used to calculated the Confidence intervals for win statistics.
}
}
\description{
Win statistics calculation using formula syntax
}
\examples{
# Example 1
calcWINS(x = GROUP ~ TRTP, data = COVID19b)
# Example 2
calcWINS(x = GROUP ~ TRTP, data = COVID19, ref = "Placebo", alpha = 0.01, WOnull = 1.2)
#' Example 3
calcWINS(x = GROUP ~ TRTP, data = COVID19)$WP
calcWINS(x = GROUP ~ TRTP, data = COVID19, SE_WP_Type = "unbiased")$WP
}
\references{
The theory of win statistics is covered in the following papers:
\itemize{
\item Win proportion and win odds confidence interval calculation:
\cr \cr Bamber D (1975) "The area above the ordinal dominance graph and the area below the receiver operating characteristic graph." Journal of Mathematical Psychology 12.4: 387-415. \url{doi:10.1016/0022-2496(75)90001-2}.
\cr \cr DeLong ER et al. (1988) "Comparing the Areas Under Two or More Correlated Receiver Operating Characteristic Curves: A Nonparametric Approach." Biometrics 44.3: 837-845. \url{doi:10.2307/2531595}.
\cr \cr Brunner E et al. (2021) "Win odds: an adaptation of the win ratio to include ties." Statistics in Medicine 40.14: 3367-3384. \url{doi:10.1002/sim.8967}.
\cr \cr Gasparyan SB et al. (2021) "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2: 580-611. \url{doi:10.1177/0962280220942558}.
\cr \cr Gasparyan SB et al. (2021) "Power and sample size calculation for the win odds test: application to an ordinal endpoint in COVID-19 trials." Journal of Biopharmaceutical Statistics 31.6: 765-787. \url{doi:10.1080/10543406.2021.1968893}.
\cr \cr Brunner E, Konietschke F. (2025) "An unbiased rank-based estimator of the Mann–Whitney variance including the case of ties." Statistical Papers 66.20. \url{doi:10.1007/s00362-024-01635-0}.
\item Win ratio: the first CI utilizes the standard error derived from the \code{gamma} statistic standard error as outlined by:
\cr \cr Gasparyan SB, Kowalewski EK, Buenconsejo J, Koch GG. (2023) "Hierarchical Composite Endpoints in COVID-19: The DARE-19 Trial." In Case Studies in Innovative Clinical Trials, Chapter 7, 95–148. Chapman; Hall/CRC. \url{doi:10.1201/9781003288640-7}.
\item Win ratio: the second CI utilizes the standard error presented by:
\cr \cr Yu RX, Ganju J. (2022) "Sample size formula for a win ratio endpoint." Statistics in Medicine 41.6: 950-63. \url{doi:10.1002/sim.9297}.
\item Goodman Kruskal's \code{gamma} and CI: matches implementation in \code{DescTools::GoodmanKruskalGamma()} and based on:
\cr \cr Agresti A. (2002) Categorical Data Analysis. John Wiley & Sons, pp. 57-59. \url{doi:10.1002/0471249688}.
\cr \cr Brown MB, Benedetti JK. (1977) "Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables." Journal of the American Statistical Association 72, 309-315. \url{doi:10.1080/01621459.1977.10480995}.
\cr \cr Goodman LA, Kruskal WH. (1954) "Measures of association for cross classifications." Journal of the American Statistical Association 49, 732-764. \url{doi:10.1080/01621459.1954.10501231}.
\cr \cr Goodman LA, Kruskal WH. (1963) "Measures of association for cross classifications III: Approximate sampling theory." Journal of the American Statistical Association 58, 310-364. \url{doi:10.1080/01621459.1963.10500850}.
}
}
\seealso{
\code{\link[=calcWINS]{calcWINS()}}, \code{\link[=calcWINS.hce]{calcWINS.hce()}}, \code{\link[=calcWINS.data.frame]{calcWINS.data.frame()}}.
}
