\name{wsci}
\alias{wsci}
\title{Within subject confidence intervals}
\description{Creates within subject confidence intervals using the methods
described in Loftus & Masson (1994) and Wright (2007).}
\usage{wsci(mat, meth = 0, interval = 95, boots = 1000, diff = 0, ...)}
\arguments{
  \item{mat}{Input should be a matrix of repeat measures}
  \item{meth}{
meth = 0 removes the individual differences and calculates the wsci
\cr meth = 1 does the same but the intervals are bootstrapped
\cr meth = 2 uses the MSsxc and produces the same estimates as L&M (1994)}
  \item{interval}{Confidence interval in percentages, so 95 is default}
  \item{boots}{Replications}
  \item{diff}{diff=0 is just WSCI, diff=1 is BSCI and WSCI}
  \item{\dots}{Further arguments}
}
\details{
Function is described in Wright (2007) 
and on http://www2.fiu.edu/~dwright/WSCI/wscihome.htm}

\value{
Returns matrix for printing}
\references{
Loftus, G. R. & Masson, M. E. J. (1994). Using confidence-intervals in 
within-subject designs. \emph{Psychonomic Bulletin & Review}, \bold{1}, 476-490.
\cr Wright, D. B. (2007). Graphing within subject confidence intervals (WSCI) 
using SPSS and S-Plus. /emph{Behavior Research Methods}, \bold{39}, 82-85.
}
\author{Daniel B. Wright}
\examples{
data(company)
wsci(company[,2:5],diff=1)}