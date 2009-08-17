\name{company}
\alias{company}
\docType{data}
\title{Company efficiency}
\description{
Data from Wright (1997, Table 6.5). Made up data. }
\usage{data(company)}
\format{
  A data frame with 6 observations on the following 5 variables.
  \describe{
    \item{\code{COMPANY}}{a numeric vector}
    \item{\code{AUTUMN}}{a numeric vector}
    \item{\code{WINTER}}{a numeric vector}
    \item{\code{SPRING}}{a numeric vector}
    \item{\code{SUMMER}}{a numeric vector}
  }
}
\source{
Wright, D. B. (1997). \emph{Understanding Statistics: An Introduction for the Social Sciences}. London : Sage. 
}
\references{
Wright, D. B. (1997). \emph{Understanding Statistics: An Introduction for the Social Sciences}. London : Sage. 
}
\examples{
data(company)
wsci(company,diff=1)
}
\keyword{datasets}
