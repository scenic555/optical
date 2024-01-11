#' Optimal calibration design for all items
#'
#' Create a plot with optimal design for each item. Optimal design is represented
#' by different colored lines for each item.
#'
#' @param yyy  an \code{\link{optical}} object; the output of a call [optical()].
#' @param linew linewidth for design.
#' @param ablim ability limits; plots will be made in the range \[-ablim, ablim].
#' @param colvec  vector of color sequence for items (default is the R-default black, red, green, etc.).
#'
#' @export drawdesign_allitems
#' @importFrom graphics abline axis legend lines mtext par points
#'            text box
#' @return An optimal design plot is displayed for all items.
#'
#'
#' @examples
#'
#' # Example No.1
#' # 2PL-model for three items with parameters (a, b) equal to (1.6, -2), (1.6, 0.5),
#' # and (1.6, 2) for the first, second, and third items, respectively.
#'
#' ip <- cbind(c(1.6, 1.6, 1.6), c(-2, 0.5, 2))
#' yyy <- optical(ip)
#' drawdesign_allitems(yyy)
#'
#'
#' \dontrun{
#' # Example No.2
#' # 2PL-models for six items; the parameters for these items are a=(1.62, 1.4, 0.98, 0.66, 0.92, 0.9),
#' # and b=(-0.47, -1.71, 0.62, -0.15, -1.71, 1.6), respectively.
#'
#' ip <- cbind(c(1.62, 1.4, 0.98, 0.66, 0.92, 0.9), c(-0.47, -1.71, 0.62, -0.15, -1.71, 1.6))
#' bid <- c(1, 1, 1, 2, 2, 2)
#' yyy <- optical(ip, bid = bid, show_progress = 2)
#' drawdesign_allitems(yyy)
#'
#' # Items of the same color that are in the same block.
#' drawdesign_allitems(yyy, colvec = bid)
#' }




drawdesign_allitems <- function(yyy, linew=NULL, ablim=4, colvec=NULL){

tot_items <- sum(sapply(yyy$ip, nrow))           # Total number of items

if(is.null(colvec)){colvec<-1:tot_items}         # Vector of colors for each item
if(is.null(linew)){linew<-(250/(tot_items+1))}   # Line thickness based on the number of items


oldpar <- par(no.readonly = TRUE)               # Get default graphical parameters
on.exit(par(oldpar))                            # Reset graphical parameters

# Increase the plot size by adjusting the margins
par(mar = c(3, 3, 2, 2) + 0.1)                  # c(bottom, left, top, right) + extra margin
plot(c(-ablim, ablim), c(1,tot_items), xlab="Ability", ylab="Items", type="n", axes=FALSE)
axis(side=1, tick=TRUE, labels=T, at=seq(-ablim, ablim, by=1), cex.axis=1.2)
axis(side=2, tick=TRUE, cex.axis=1.2,at=seq(1, tot_items, by=1), cex.axis=1.2, las=1)
axis(side=3, tick=TRUE, labels=T, at=seq(-ablim, ablim, by=1), cex.axis=1.2)
axis(side=4, tick=TRUE, cex.axis=1.2,at=seq(1, tot_items, by=1), cex.axis=1.2, las=1)
box()
mtext("Ability", side=1, line=2, cex=1.2)
mtext("Items",   side=2, line=2, cex=1.2)



#yp<-c()
for(j in 1:length(yyy$ht)){
  imd <- apply(yyy$dd[[j]], 2, which.min)   # indicator for minimal directional derivative
  ip <- yyy$ip[[j]]
  m <- dim(yyy$ip[[j]])[1]
  itemnum <- rownames(ip)                   # item number as character
  ypoint <- as.numeric(itemnum)             # item number as numeric


  p <- length(imd)
  x <- cbind(yyy$t[[j]], imd)
  u <- rbind(c(-8, 0), x, c(8, imd[p]))

  for(s in 1:length(ypoint)){
    w <- u
    w[!w[,2]==s,2] <- 0                     # All other places design value is zero
    w[w[,2]==s,2] <- ypoint[s]              # Design value is equal to item number

    for (i in 2:dim(w)[1]) { lines(c(w[(i-1), 1], w[i, 1]),c(ypoint[s], ypoint[s]), col=colvec[w[i, 2]], lend=1, lwd=linew)}
  }
    #yp <- c(yp, ypoint)                    # Save y points

}
    #lb <- paste0("item", yp, sep="")       # Generate item number
    # axis(2, at=yp, labels=lb)             # Y-axis tick label as item number
}



