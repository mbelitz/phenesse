% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weib_percentile_ci.R
\name{weib_percentile_ci}
\alias{weib_percentile_ci}
\title{Calculating the CIs of a percentile estimate of a seasonal abundance distribution
using the non-parametric bootstrapping.}
\usage{
weib_percentile_ci(observations, iterations, percentile, bootstraps)
}
\arguments{
\item{observations}{is a vector of dates/time of observations given as numeric values}

\item{iterations}{is the number of iterations you want to run to create empirical
bootstrapping to estimate bias of original CDF. The bias is used to calculate
a bias corrected estimate of the percentile bound.}

\item{percentile}{is the percentile of the cumulative distribution function of interest}

\item{bootstraps}{is the number of bootstraps you want to run to create the CIs}
}
\description{
\code{weib_percentile_ci} uses non-parametric bootstrapping from the boot package
to estimate 95% CIs
}
\examples{

# Gather last 10 sightings of dodo
observations <- c(1662, 1638, 1631, 1628, 1628, 1611, 1607, 1602, 1601, 1598)

# Estimate when 90\% of dodo were left with 95\% CI
weib_percentile_ci(observations, iterations = 100, percentile = 0.9, bootstraps = 100)
}
\keyword{percentile}
\keyword{phenology,}
\keyword{weibull,}