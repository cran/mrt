\name{juryrd}
\alias{juryrd}
\docType{data}
\title{Calculating Reasonable Doubt}
\description{
  These data (Wright & Hall, 2006) are used to show logistic regression and calculating
  reasonable doubt (like LD-50 in biostats ... Lethal Dose-50). The MASS
  package (or VR bundle) has a function for the confidence interval.
}
\usage{data(juryrd)}
\format{
  A data frame with 172 observations on the following 4 variables.
  \describe{
    \item{\code{SUBNO}}{a numeric vector}
    \item{\code{GUILTY}}{a numeric vector}
    \item{\code{FORM}}{a numeric vector}
    \item{\code{BELIEF}}{a numeric vector}
  }
}
\references{
 	Wright, D. B., & Hall, M. (2007). How a "Reasonable Doubt" instruction
   affects decisions of guilt.
   \emph{Basic and Applied Social Psychology}, \bold{29}, 85-92.
}
\keyword{datasets}
