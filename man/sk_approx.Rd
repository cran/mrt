\name{sk_approx}
\alias{sk_approx}
\title{Old approx to estimate standard error skewness}
\description{
This is the equation, sqrt(6/length(x)), in some textbooks. It is called in the 
function sk. This is really just for comparison. }
\usage{sk_approx(x)}
\arguments{
  \item{x}{The variable of interest}}
\author{Daniel B. Wright)}
\seealso{sk}
\examples{
varx <- runif(20)^2
sk_approx(varx)
}
\keyword{skewness}
