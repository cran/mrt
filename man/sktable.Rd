\name{sktable}
\alias{sktable}
\title{Skewness and Kurtosis Table}
\description{
A table of traditional statistics for skewness and kurtosis values, with their
asymptotic and bootstrap standard errors (Wright & Herrington, 2011). }
\usage{
sktable(varx, reps = 2000, bootm = TRUE, ...)
}
\arguments{
  \item{varx}{Variable to analyzed}
  \item{reps}{Bootstrap replications}
  \item{bootm}{To bootstrap or not}
  \item{\dots}{Further arguments}
}
\references{  
Wright, D.B. & Harrington, J.A. (2011, actual in press at the moment). 
Problematic standard errors and confidence intervals for skewness and 
kurtosis. \emph{Behavior Research Methods}. www2.fiu.edu/~dwright/skewkurt
}
\author{Daniel B. Wright}
\examples{
varx <- runif(20)^2
sktable(varx)
}
