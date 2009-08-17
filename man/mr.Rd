\name{mr}
\alias{mr}
\title{mr  Internal function for moments}
\description{
  This is an internal function for sktable. Other functions for moments exist}
\usage{mr(x, n, r)}
\arguments{
  \item{x}{The variable}
  \item{n}{The sample size (calculated in different function for speed purposes)}
  \item{r}{Which moment}
}
\author{Daniel B. Wright}
\examples{
xvar <- runif(20)^2
mr(xvar,20,4)
}
