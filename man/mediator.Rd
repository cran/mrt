\name{mediator}
\alias{mediator}
\title{Simple mediator analysis and graph}
\description{
  A function that conducts a simple mediation analysis and makes the figure
  shown in Wright and London (2009). }
\usage{
mediator(x, y, m, ...)
}
\arguments{
  \item{x}{ The predictor variable }
  \item{y}{ The response variable }
  \item{m}{ The mediator }
  \item{\dots}{ Other arguments }
}
\value{
 The graph is the main output.
}
\author{Daniel B. Wright}
\note{
 There are other mediation packages. This was shown in Wright and London
 to illustrate how to make a function. It does not do anything particularly
 novel or clever.
}
\examples{
set.seed(143)
leaflet <- rep(c(0,1),each=50)
fairskin <- rbinom(100,1,.5)
likely <- rbinom(100,10,.20 + .2*leaflet + .2*fairskin)
plan <- rbinom(100,7,likely/15+leaflet*.2)
mediator(leaflet,plan,likely)
}
