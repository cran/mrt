\name{b1se2}
\alias{b1se2}
\title{b1se2 statistic for standard error of skewness}
\description{A common statistic for standard error of skewness. 
Called by the function sktable. Uses Kendall, Stuart and Ord (1987, p. 344).}
\usage{b1se2(x)
}
\arguments{
  \item{x}{The variable of interest}}
\details{see Wright and Herrington (2011)}
\references{
Kendall, M., Stuart, A., & Ord, J.K. (1987). \emph{Kendall's advanced theory of 
statistics: Vol. 1. Distribution theory}. New York: Oxford University Press.
\cr Wright, D.B. & Harrington, J.A. (2011, actual in press at the moment). 
Problematic standard errors and confidence intervals for skewness and 
kurtosis. \emph{Behavior Research Methods}. www2.fiu.edu/~dwright/skewkurt
}
\author{Daniel B. Wright}
\note{While this can be called on its own, it was written to be used by sktable.}
\seealso{sktable}
\examples{
varx <- runif(20)^2
b1se2(varx)
}
\keyword{skewness}
