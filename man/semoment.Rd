\name{semoment}
\alias{semoment}
\title{SE moments}
\description{The standard errors for moments as defined in eqn 10.9 
from Stuart and Ord, 1987.}
\usage{
semoment(x, r = 4)
}
\arguments{
  \item{x}{Variable of interest}
  \item{r}{Which moment}
}
\references{
Kendall, M., Stuart, A., & Ord, J.K. (1987). \emph{Kendall's advanced theory of 
statistics: Vol. 1. Distribution theory}. New York: Oxford University Press.
}
\author{Daniel B. Wright}
\seealso{sktable}
\examples{
varx <- runif(20)^2
semoment(varx)
}
