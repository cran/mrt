\name{ngfinder}
\Rdversion{1.1}
\alias{ngfinder}
\title{Nominal group finder (Kelley & Wright, 2010, BRM)}
\description{
 Finds the set of nominal groups which is representative of all possible sets
 for either mean, variance (and other moments) or the distribution. Details
 are in Kelley and Wright (2010), where a C++ function also described.}
\usage{
ngfinder(nomg, number = 2, right = 1, replic = 1000, moments = NULL, distmeth = "ks", ...)
}
\arguments{
  \item{nomg}{     The data file. One line per participant}
  \item{number}{     The size of the groups}
  \item{right}{     The value for a correct answer}
  \item{replic}{    How many samples to draw}
  \item{moments}{     If matching on moments, how many to use. 2 is recommended}
  \item{distmeth}{     If matching on distributions, whether to use Kolmogorov-Smirnov
   or Pearson method}
  \item{\dots}{     Other arguments}}
\details{
  The function takes a data matrix for a control condition in a collaboration
  study, and finds a set which is really close to the average of all possible
  sets. Details are in Kelley and Wright (2010), which is an advance on 
  Wright (2007).}
\value{
  \item{forgroups }{  The numbers for the optimal groups, so the participant numbers}
  \item{values }{  The number of correct responses for each of the groups}
}
\references{
     Kelley, M. R. & Wright, D.B. (2010) Obtaining representative nominal groups. 
     \emph{Behavior Research Methods}, \bold{42}, 36-41.
     \cr Wright, D.B. (2007). Calculating nominal group statistics in collaboration studies.
     \emph{Behavior Research Methods}, \bold{39}, 460-470.
     \cr http://www2.fiu.edu/~dwright/nomg/index.htm
}
\author{Daniel B. Wright, dwright@fiu.edu}
\examples{
set.seed(32)
dmat <- rbinom(120,1,.4)
dim(dmat) <- c(12,10)
ngfinder(dmat,number=3,replic=4)
ngfinder(dmat,number=3,replic=4,moments=3)}
