\name{b1se}
\alias{b1se}
\title{b1se statistic for Standard error of skewness}
\description{A common statistic for se of skewness. Called by the function sktable}
\usage{b1se(x)}
\arguments{
  \item{x}{The variable of interest}}
\details{see Wright and Herrington (2011)}
\references{
Wright, D.B. & Harrington, J.A. (2011, actual in press at the moment). 
Problematic standard errors and confidence intervals for skewness and 
kurtosis. \emph{Behavior Research Methods}. www2.fiu.edu/~dwright/skewkurt
}
\author{Daniel B. Wright}
\note{While this can be called on its own, it was written to be used by sktable.}
\seealso{sktable}
\examples{
varx <- runif(20)^2
b1se(varx)
}
\keyword{skewness}
