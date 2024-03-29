% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convergenceplot.R
\name{convergenceplot}
\alias{convergenceplot}
\title{Convergence plot}
\usage{
convergenceplot(yyy, refline = c(0.002, 1e-05))
}
\arguments{
\item{yyy}{an \code{\link{optical}} object; the output of a call \code{\link[=optical]{optical()}}.}

\item{refline}{reference line.}
}
\value{
A convergence plot is displayed.
}
\description{
For each block, convergence plots display efficiency of design, violations
of the equivalence theorem, the step length used vs. iteration number.
These plots are suitable for monitoring the convergence of the optimal item
calibration algorithm.
}
\details{
Convergence plots have three panels.
\itemize{
\item {First panel monitors efficiency of design vs. iteration number.}
\item {Second panel monitors violations of equivalence theorem vs. iteration number.}
\item {Third panel monitors step size used vs. iteration number.}
}
}
\examples{
# Example No.1
# 2PL-model for three items with parameters (a, b) equal to (1.6, -2), (1.6, 0.5),
# and (1.6, 2) for the first, second, and third items, respectively.

ip <- cbind(c(1.6, 1.6,1.6),c(-2, 0.5,2))
yyy <- optical(ip)
convergenceplot(yyy, refline=c(0.002, 0.001*0.005/0.45))

\dontrun{

# Example No.2
# 2PL-models for six items; parameters a=(1.62, 1.4, 0.98, 0.66, 0.92, 0.9),
# and b=(-0.47, -1.71, 0.62, -0.15, -1.71, 1.6), respectively.

a<-c(1.62, 1.4,0.98,0.66,0.92,0.9)
b<-c(-0.47,-1.71,0.62,-0.15,-1.71,1.6)
ip<-cbind(a,b)
bid<-c(1,1,1,2,2,2)
yyy <- optical(ip,bid=bid,show_progress=2)
convergenceplot(yyy, refline=c(0.002, 0.001*0.005/0.45))

}
}
\seealso{
\code{\link{drawdesign}}, \code{\link{drawdesign_allitems}}
}
