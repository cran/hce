% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcWO_hce.R
\name{calcWO.hce}
\alias{calcWO.hce}
\title{Win odds calculation for \code{hce} objects}
\usage{
\method{calcWO}{hce}(x, ...)
}
\arguments{
\item{x}{an \code{hce} object.}

\item{...}{additional parameters.}
}
\value{
a data frame containing the win odds and its confidence interval. It contains the following columns:
\itemize{
\item WO calculated win odds.
\item LCL lower confidence limit.
\item UCL upper confidence limit.
\item SE standard error of the win odds.
\item WOnull win odds of the null hypothesis (specified in the \code{WOnull} argument).
\item alpha two-sided significance level for calculating the confidence interval (specified in the \code{alpha} argument).
\item Pvalue p-value associated with testing the null hypothesis.
\item WP calculated win probability.
\item LCL_WP lower confidence limit for \code{WP}.
\item UCL_WP upper confidence limit for \code{WP}.
\item SE_WP standard error of the win probability.
\item SD_WP standard deviation of the win probability, calculated as \code{SE_WP} multiplied by \code{sqrt(N)}.
\item N total number of patients in the analysis.
}
}
\description{
Win odds calculation for \code{hce} objects
}
\examples{
Rates_A <- c(1, 1.5) 
Rates_P <- c(2, 2) 
dat <- simHCE(n = 500, TTE_A = Rates_A, TTE_P = Rates_P, CM_A = 1.25, CM_P = 1)
calcWO(dat)
calcWO(dat, ref = "A", WOnull = 1, alpha = 0.01)
}
\references{
Gasparyan SB et al. "Adjusted win ratio with stratification: calculation methods and interpretation." Statistical Methods in Medical Research 30.2 (2021): 580-611. \url{doi:10.1177/0962280220942558}
}
\seealso{
\code{\link[=calcWO]{calcWO()}}, \code{\link[=calcWO.formula]{calcWO.formula()}}, \code{\link[=calcWO.data.frame]{calcWO.data.frame()}}.
}
