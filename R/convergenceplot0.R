################################################################################
#  Convergence plots
#  First panel monitors efficiency of design vs. iteration number
#  Second panel monitors violations of equivalence theorem vs. iteration number
#  Third panel monitors step size used vs. iteration number
#  Maybe add following in future version:
#  Fourth panel shows final grid
#################################################################################
#  Convergence plot
#
#  Convergence plots displaying efficiency of design, violations of equivalence
#  theorem, and alpha(Step length used) vs. iteration number. These plot are
#  suitable for monitoring the convergence of optimal item calibration algorithm.
#
#
#  @param yyy   a \code{\link{optical}} object; the output of a call [optical()]
#  @param refline  reference line
#
#  @details Convergence plots have three panel.
#  \itemize{
#  \item {First panel monitors efficiency of design vs. iteration number}
#  \item {Second panel monitors violations of equivalence theorem vs. iteration number}
#  \item {Third panel monitors step size used vs. iteration number}
#   }
#
#  @return A convergence plot is displayed.
#
#  @seealso \code{\link{drawdesign}}
#
#  @export convergenceplot
#
#  @examples
#  2PL-models for two items; parameters (a, b)=(1.6, -1) and (1.6, 1), respectively
#  ip <- cbind(c(1.6, 1.6),c(-1, 1))
#
#  yyy <- optical(ip)
#
#  convergenceplot(yyy, refline=c(0.002, 0.001*0.005/0.45))


convergenceplot0<- function(yyy, refline=c(0.002, 0.00001)) {

 moo=yyy$mooiter

 # Save the current par settings
 oldpar <- par(no.readonly = TRUE)
 on.exit( par(oldpar))  # reset graphical parameters

 par(mfrow=c(3, 1), oma=c(1, 1, 0, 1), mar=c(2, 3, 0.5, 0)) # for combined graph


 ti  <- length(moo[, 1])  # total number of iterations
  nil <- c()               # number of iteration when new inner loop (nil) begins
  for (i in 2:ti) {
    if (moo[i-1, 1] < moo[i, 1]) { nil <- c(nil, i-0.5) }
  }

  # Panel 1
  tis <- min(ti/3, 51)  # change the y-axis such that all results from iteration 51 are visible (or from total iterations/3 if earlier)
  plot(c(1, ti), c(min(moo[tis:ti, 3]), max(moo[, 3])), type="n", xlab="", ylab="", axes=FALSE)
  axis(side=1, tick=TRUE, labels=T, cex.axis=1.2)
  axis(side=2, tick=TRUE, cex.axis=1.2)
  #mtext("Iteration number", side=1, line=-1.0, cex=1.1)
  mtext("Efficiency (vs. rand.des)", side=2, line=2.3, cex=1.1)
  lines(1:ti, moo[, 3], col=moo[, 1], lwd=2)
  abline(v=nil, lty=2)

  # Panel 2
  plot(c(1, ti), c(min(c(moo[, 4], 0.001)), max(moo[3:ti, 4])), type="n", log="y", xlab="", ylab="", axes=FALSE)
  axis(side=1, tick=TRUE, labels=T, cex.axis=1.2)
  axis(side=2, tick=TRUE, at=c(0.001, 0.1, 10, 1000), cex.axis=1.2)
  #mtext("Iteration number", side=1, line=-1.0, cex=1.1)
  mtext("Violation of eq.th.", side=2, line=2.3, cex=1.1)
  lines(1:ti, moo[, 4], col=moo[, 1], lwd=2)
  abline(v=nil, lty=2)
  abline(h=refline[1], lty=2)

  # Panel 3
  plot(c(1, ti), c(min(c(moo[, 5], 0.001)), max(moo[, 5])), type="n", log="y", xlab="", ylab="", axes=FALSE)
  axis(side=1, tick=TRUE, labels=T, cex.axis=1.2)
  axis(side=2, tick=TRUE, at=c(0.001, 0.01, 0.1, 1), cex.axis=1.2)
  mtext("Iteration number", side=1, line=-1.0, cex=1.1)
  mtext("Step length used", side=2, line=2.3, cex=1.1)
  lines(1:ti, moo[,5], col=moo[,1], lwd=2)
  abline(v=nil, lty=2)
  abline(h=refline[2], lty=2)

}

