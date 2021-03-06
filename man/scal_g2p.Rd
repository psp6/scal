% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scal_g2p.R
\name{scal_g2p}
\alias{scal_g2p}
\title{Calculates sample size for comparing two proportions}
\usage{
scal_g2p(p1, p2, alpha, power)
}
\arguments{
\item{alpha}{level for the confidence interval}

\item{\sigma}{expected variability of the outcome}

\item{d}{precision or margin of the error for the outcome}
}
\value{
output Sample size
}
\description{
The function requires 1) expected prop in two groups 2) desired significance level 3)
level of power as required input.
}
\examples{
scal_g2p(10, 30, 0.05, 0.80)


}
\keyword{,}
\keyword{means,}
\keyword{precision}

