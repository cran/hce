% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_hce.data.frame.R
\name{as_hce.data.frame}
\alias{as_hce.data.frame}
\title{Coerce a data frame to an \code{hce} object}
\usage{
\method{as_hce}{data.frame}(x, ...)
}
\arguments{
\item{x}{a data frame.}

\item{...}{additional parameters.}
}
\value{
an \code{hce} object.
}
\description{
Coerce a data frame to an \code{hce} object
}
\examples{
KHCE <- as_hce(KHCE)
calcWO(KHCE)
}
\seealso{
\code{\link[=as_hce]{as_hce()}}, \code{\link[=as_hce.default]{as_hce.default()}}.
}
