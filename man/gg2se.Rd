\name{gg2se}
\alias{gg2se}
\title{G2se statistic for standard error of kurtosis}
\description{A common statistic for standard error of kurtosis. Called by the function sktable}
\usage{gg2se(x)
}
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
gg2se(varx)
}
\keyword{kurtosis}
