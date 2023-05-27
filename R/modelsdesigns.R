########################################################################################################################################
# Draw models together with several designs
#  ip:    matrix with item parameters for all items (number of rows determines number of items;
#         number of column 2 (2PL) or 3 (3PL or mixed 2/3-PL with NA for 2PL-items in third column)
#  yh:    list of designs (first column = interval boundaries, second column = item number up to this boundary
#  ablim: ability limits; plots will be made in the range [-ablim, ablim]
#  legi:  legend for items (string)
#  legd:  legend for designs (vector of strings; same length as yh)
########################################################################################################################################

#' @importFrom grDevices dev.new

modelsdesigns <- function(ip, yh, ablim=4, legi=NA, legd=NA){
  m   <- dim(ip)[1]                     # number of items
  mod <- dim(ip)[2]                     # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
  nd  <- length(yh)                     # number of designs to be shown
  dev.new(width=8, height=5, unit="cm")

  oldpar <- par(oma=c(1, 1, 0, 1), mar=c(2, 3, 0.5, 0), fig=c(0, 1, 0.5, 1))  # first panel
  oldpar <- par(no.readonly = TRUE)
  on.exit( par(oldpar))  # reset graphical parameters
  plot(c(-ablim, ablim), c(0, 1), type="n", xlab="Ability", ylab="", las=1)
  text(ablim-1, 0.15, legi)
  t <- seq(-ablim, ablim, by=0.01)
  for (i in 1:m){
    a <- ip[i, 1]
    b <- ip[i, 2]
    if (mod==3 && is.na(ip[i, 3])==FALSE)  c <- ip[i, 3]  else  c <- 0
    lines(t, c + (1-c)/(1+exp(-a*(t-b))), col=i, lwd=3)
  }
  par(oma=c(1, 1, 0, 1), mar=c(0, 3, 0.5, 0), fig=c(0, 1, 0, 0.5), new=TRUE)  # second panel

  plot(c(-ablim, ablim), c(0, 1), type="n", xlab="", ylab="", axes=FALSE)
  lwd0 <- 48/nd
  for (i in 1:nd){
    yyh <- rbind(c(-ablim-1, 0), yh[[i]])
    for (j in 2:dim(yyh)[1]) { lines(c(yyh[(j-1), 1], yyh[j, 1]), rep(1-(i-0.5)/nd, 2), col=yyh[j, 2], lend=1, lwd=lwd0) }
    mtext(legd[i], side=2, at=1-(i-0.5)/nd, las=1, adj=1.1)
  }
}
