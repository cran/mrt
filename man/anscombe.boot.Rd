\name{anscombe.boot}
\alias{anscombe.boot}
\title{Anscombe-Glynn (1983) test with bootstrap of z}
\description{
Conducts the Anscombe-Glynn (1983) test for kurtosis which uses 
the Wilson-Hilferty transformation. Uses the anscombe.test function from 
moments (Komsta & Novomestky, 2007), but the equations can be taken straight 
from Anscombe and Glynn (1983). That function produces a z value. Here, this 
statistic is bootstrapped. The upper and lower bounds of z are produced. 
Requires the package boot (Canty & Ripley, 2010).}
\usage{
anscombe.boot(x, reps = 2000, printtest = FALSE, clevel = 0.95, method = "bca", ...)
}
\arguments{
  \item{x}{The variable of interest}
  \item{reps}{The number of bootstrap replications}
  \item{printtest}{Whether to print the results of anscombe.test}
  \item{clevel}{The confidence level of boot.ci}
  \item{method}{Any argument other than "bca" will produce percentile intervals.}
  \item{\dots}{Other arguments}
}
\value{
  \item{Kurtosis}{g2}
  \item{z}{z from anscombe.test}
  \item{lb}{lower bound of z}
  \item{ub}{upper bound of z}
}
\references{
Anscombe, F.J. & Glynn, W.J. (1983) Distribution of kurtosis 
statistic for normal statistics. \emph{Biometrika}, \bold{70}, 227-234.
\cr Canty, A. & Ripley, B. (2010). boot: Bootstrap R (S-Plus) functions. 
R package version 1.2-42. 
\cr Komsta, L. & Novomestky, F. (2007). Moments: Moments, cumulants, skewness, 
kurtosis and related tests. 
R package version 0.11. http://www.r-project.org, http://www.komsta.net/}
\author{Daniel B. Wright}
\seealso{anscombe.test}
\examples{
varx <- runif(100)
anscombe.boot(varx)
anscombe.boot(varx,printtest=TRUE,method="other",reps=600)
}
\keyword{kurtosis}
\keyword{Anscombe}
