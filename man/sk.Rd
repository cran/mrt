\name{sk}
\alias{sk}
\title{Skewness and Kurtosis statistics, and test of normality}
\description{
This function runs sktable to provide tradition estimates of skewness and 
kurtosis with asymptotic and bootstrap standard errors (Wright & Herrington,
2011). It also runs Shapiro-Wilk and Kolmogor-Smirnov tests. The package 
nortest (Gross, no date) has further tests of normality.}
\usage{sk(varx, table = TRUE, SW = TRUE, KS = FALSE, rough = FALSE, ...)}
\arguments{
  \item{varx}{Variable of interest}
  \item{table}{if FALSE sktable will not be used and no table produced}
  \item{SW}{Run Shapiro-Wilk}
  \item{KS}{Run Kolmogorov-Smirnov}
  \item{rough}{Include rough/approx standard errors from old textbooks}
  \item{\dots}{Further arguments}
}
\references{
Gross, J. (no date). nortest: Tests for normality. R package version 1.0.
\cr Wright, D.B. & Harrington, J.A. (2011, actual in press at the moment). 
Problematic standard errors and confidence intervals for skewness and 
kurtosis. \emph{Behavior Research Methods}. www2.fiu.edu/~dwright/skewkurt
}
\author{Daniel B. Wright}
\seealso{sktable}
\examples{
varx <- runif(20)^2
sk(varx)
}