#' Graph for (optimal) design
#'
#' Generate plot for optimal design with four possible layouts. All layouts have design
#' first incl. efficiency vs. random design; then line with item having minimal
#' directional derivative
#'
#' @param yyy  a \code{\link{optical}} object; the output of a call [optical()]
#' @param ip   matrix with item parameters for all items (number of rows
#'             determines number of items; number of column 2 (2PL) or
#'             3 (3PL or mixed 2/3-PL with NA for 2PL-items in third column).
#' @param ablim ability limits; plots will be made in the range \[-ablim, ablim]
#' @param ylowl  y low level (minimum value of directional derivative shown in the plot)
#' @param refline  reference line correspond to desired minimum violation of equivalence theorem
#' @param textout  If textout=TRUE (default), the item parameters will be printed if number of items $<5$
#'                 and the efficiency vs. the random design; if textout=FALSE, no such text is printed
#' @param itemnum number of items
#' @param layout  layouts of plots
#' \itemize{
#'   \item{Layout 1:} {third panel has directional derivatives (cut at ylowl or lowest value of dirdev)}
#'   \item{Layout 2:} {third panel has violations of equivalence theorem, should be ideally small. Stopping criterion could be <0.002 (refline)}
#'   \item{Layout 3:} {third panel monitors efficiency of design vs. iteration number}
#'   \item{Layout 4:} {third panel monitors violations of equivalence theorem vs. iteration number}
#'   \item{Layout 5:} {third panel shows item characteristic curves}
#'   \item{Layout 0:} {only one panel with design}
#'   }
#' @param colvec  vector of color sequence for items (default is the R-default black, red, green, etc.)
#'
#' @seealso \code{\link{convergenceplot}}
#'
#' @export drawdesign
#' @importFrom graphics abline axis legend lines mtext par points
#'            polygon text
#'
#' @examples
#' # 2PL-models for two items; parameters (a, b)=(1.6, -1) and (1.6, 1), respectively
#' ip <- cbind(c(1.6, 1.6),c(-1, 1))
#'
#' yyy <- optical(ip, oc="D", uncert=FALSE, ipop,
#'                imf=c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.45),
#'                maxiter=rep(300, 6), eps=rep(0.002, 6),
#'                nnn=c(0, 50, 50, 200, 200, 200),
#'                nsp=c(0.001, 0.0001, 0.0001, 0.00001, 0.00001, 0.00001),
#'                sss=0.001, falpha=1.08, ig=3, ex=0)
#'
#' drawdesign(yyy=yyy, ip=ip, ylowl=-1000, refline=0.002, layout=1)



drawdesign <- function(yyy, ip, ablim=7, ylowl=-9999999, refline=0.002, textout=TRUE, itemnum=NA, layout=1, colvec=1:12) {
  imd <- apply(yyy$dd, 2, which.min)    # indicator for minimal directional derivative
  m   <- dim(ip)[1]                     # number of items
  if (is.na(itemnum[1])) itemnum <- 1:m
  mod <- dim(ip)[2]                     # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
  np  <- length(which(!is.na(ip)))      # number of parameters
  h   <- rbind(c(-8, 0, 0, 0), yyy$h1)
  k   <- dim(h)[1]                      # number of intervals to be drawn + 1
  moo <- yyy$mooiter                    # monitored iterations
  z1  <- h[, 1]
  z2  <- h[, 3]

  if (layout>0) {
    #dev.new(width=8, height=5, unit="cm")
    oldpar <- par(oma=c(1, 1, 0, 1), mar=c(2, 3, 0.5, 0), fig=c(0, 1, 0.53, 1))  # for combined graph
  }
  else {
    #dev.new(width=8, height=2.5, unit="cm")
    oldpar <- par(oma=c(1, 1, 0, 1), mar=c(2, 3, 0.5, 0))
  }

  # Normal distribution plot (Plot 1)
  x11 <- seq(-ablim, ablim, length=1001)
  y11 <- dnorm(x11)
  plot(x11, y11, type="l", lwd=1, axes=FALSE, xlab="",ylab="")
  axis(side=1, tick=TRUE, labels=FALSE, at=seq(-ablim, ablim, by=1))
  mtext("Ability", side=1, line=0.53, cex=1.2)

  # intervals filled with colours
  for (i in 2:k) {
    cord.x1 <- c(z1[i-1], seq(z1[i-1], z1[i], 0.001), z1[i])
    cord.y1 <- c(-0.02, dnorm(seq(z1[i-1], z1[i], 0.001)), -0.02)
    polygon(cord.x1, cord.y1, col=colvec[z2[i]], border = NA, lend=1)
  }

  # labeling of all colours in plot
  if (m<5 && textout) {
    if (mod==2) { ln<-c(paste0("Item ", itemnum, " (a=", round(ip[, 1], 3), ", ", "b=", round(ip[, 2], 3), ")\n")) }
    if (mod==3) { ln<-c(paste0("Item ", itemnum, " (a=", round(ip[, 1], 3), ", ", "b=", round(ip[, 2], 3), ", c=", round(ip[, 3], 3), ")\n")) }
  } else {
    ln<-c(paste0("Item ", itemnum))
  }
#legend("topleft", box.lty=0, inset=.005, ln, col=colvec[1:m], lwd=4, cex=0.75, horiz=FALSE, adj=c(0, 0.4))

legend(x=-9,y=0.40, box.lty=0, inset=.005, ln,
         col=colvec[1:m], lwd=4, cex=0.75, horiz=FALSE, adj=c(0, 0.4)
         ,xpd=TRUE,bty='n')

  # Efficiency versus random design
  ti   <- length(moo[, 1])
  effi <- moo[ti, 3]
  if (textout) legend("topright", legend=paste0(yyy$oc, "-efficiency vs.\n", "random design = ", round(effi, 3)), box.lty=0, cex=0.95)

  if (layout>0) {
    par(fig=c(0, 1, 0.48, 0.52), oma=c(0, 1, 0, 1), mar=c(0, 3, 0, 0), new=TRUE)
    #draw the interval based on where dd are minimum (Plot 2)
    p <- length(imd)
    x <- cbind(yyy$t, imd)
    u <- rbind(c(-8, 0), x, c(8, imd[p]))
    plot(c(-ablim, ablim), c(-0.1, 0.1), type="n", axes=FALSE)
    for (i in 2:dim(u)[1]) { lines(c(u[(i-1), 1], u[i, 1]), c(0, 0), col=colvec[u[i, 2]], lend=1, lwd=8) }

    par(fig=c(0, 1, 0, 0.47), oma=c(1, 1, 0, 1), mar=c(2, 3, 0.5, 0), new=TRUE)
    # Plot 3
    if (layout==1) {
      #directional derivative plot box
      plot(c(-ablim, ablim), c(max(ylowl, min(yyy$dd)), np+0.5), type="n", axes=FALSE)
      axis(side=1, tick=TRUE, labels=T, at=seq(-ablim, ablim, by=1), cex.axis=1.2)
      axis(side=2, tick=TRUE, cex.axis=1.2)
      mtext("Directional derivative", side=2, line=2.3, cex=1.2)

      #draw directional derivative lines
      for (i in 1:m){lines(yyy$t, yyy$dd[i, ], col=colvec[i], lwd=2)}
    }
    if (layout==2) {
      viol <- idwv(yyy$dd, yyy$xi)$vio
      plot(c(-ablim, ablim), c(max(max(viol), refline)*1.1, 0), type="n", axes=FALSE)
      axis(side=1, tick=TRUE, labels=T, at=seq(-ablim, ablim, by=1), cex.axis=1.2)
      axis(side=2, tick=TRUE, cex.axis=1.2)
      mtext("Violation of eq.th.", side=2, line=2.3, cex=1.2)
      points(yyy$t, viol, col=colvec[yyy$xi])
      abline(h=refline, col=2)
    }
    if (layout == 3) {
      nil <- c()   # number of iteration when new inner loop (nil) begins
      for (i in 2:ti) {
        if (moo[i-1, 1] < moo[i,1]) { nil <- c(nil, i-0.5) }
      }
      tis <- min(ti/3, 51)  # change the y-axis such that all results from iteration 51 are visible (or from total iterations/3 if earlier)
      plot(c(1, ti), c(min(moo[tis:ti, 3]), max(moo[, 3])), type="n", xlab="Iteration number", axes=FALSE)
      axis(side=1, tick=TRUE, labels=T, cex.axis=1.2)
      axis(side=2, tick=TRUE, cex.axis=1.2)
      mtext("Iteration number", side=1, line=-1.0, cex=1.2)
      mtext("Efficiency (vs. random design)",side=2, line=2.3, cex=1.2)
      lines(1:ti, moo[, 3], col=colvec[moo[, 1]], lwd=2)
      abline(v=nil, lty=2)
    }
    if (layout == 4) {
      nil <- c()   # number of iteration when new inner loop (nil) begins
      for (i in 2:ti) {
        if (moo[i-1, 1] < moo[i, 1]) { nil <- c(nil, i-0.5) }
      }
      moo[(moo[, 4] < min(refline)/100), 4] <- min(refline)/100
      plot(c(1, ti), c(min(c(moo[, 4], 0.001)), max(moo[3:ti, 4])), type="n", xlab="Iteration number", ylab="Violation", log="y", axes=FALSE)
      axis(side=1, tick=TRUE, labels=T, cex.axis=1.2)
      axis(side=2, tick=TRUE, at=c(0.001, 0.1, 10, 1000), cex.axis=1.2)
      mtext("Iteration number", side=1, line=-1.0, cex=1.2)
      mtext("Violation of eq.th.", side=2, line=2.3, cex=1.2)
      lines(1:ti, moo[, 4], col=colvec[moo[, 1]], lwd=2)
      abline(v=nil, lty=2)
      abline(h=refline, lty=2)
    }
    if (layout == 5) {
      plot(c(-ablim, ablim), c(0, 1), type="n", xlab="Ability", ylab="", las=1)
      #text(3, 0.15, legi)
      t <- seq(-ablim, ablim, by=0.01)
      for (i in 1:m){
        if (is.na(ip[2, 1])) { a <- ip[1, 1] } else { a <- ip[i, 1] }
        b <- ip[i, 2]
        if (mod==3 && is.na(ip[i, 3])==FALSE)  c <- ip[i, 3]  else  c <- 0
        lines(t, c + (1-c)/(1+exp(-a*(t-b))), col=colvec[i], lwd=3)
      }
    }
  }
  par(oldpar)  # reset graphical parameters
}
