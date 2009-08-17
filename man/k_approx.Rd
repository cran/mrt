\name{k_approx}
\alias{k_approx}
\title{Old approx to estimate standard error kurtosis}
\description{
This is the equation, sqrt(24/length(x)), in some textbooks. It is called in the 
function sk. This is really just for comparison. }
\usage{k_approx(x)}
\arguments{
  \item{x}{The variable of interest}}
\author{Daniel B. Wright)}
\seealso{sk}
\examples{
varx <- runif(20)^2
k_approx(varx)
}
\keyword{kurtosis}
