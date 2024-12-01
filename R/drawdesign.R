#' Graph for (optimal) design in each block
#'
#' Generates a plot for the optimal design within each block, showcasing six possible layouts.
#' All layouts feature the design first, including efficiency versus random design,
#' followed by a line representing the item with the minimal directional derivative.
#'
#' @param yyy  an \code{\link{optical}} object; the output of a call [optical()].
#'
#' @param ablim ability limits; plots will be made in the range \[-ablim, ablim].
#'
#' @param ylowl  y low level (minimum value of directional derivative shown in the plot).
#'
#' @param refline  reference line corresponding to desired minimum violation of equivalence theorem.
#'
#' @param textout  If textout=TRUE (default), the item parameters will be printed if number of items $<5$
#'                 and the efficiency vs. the random design; if textout=FALSE, no such text is printed.
#' @param itemnum number of items.
#' @param layout  layouts of plots.
#' \itemize{
#'   \item Layout 1: third panel has directional derivatives (cut at ylowl or lowest value of dirdev).
#'   \item Layout 2: third panel has violations of equivalence theorem, should be ideally small. Stopping criterion could be <0.002 (refline).
#'   \item Layout 3: third panel monitors efficiency of design vs. iteration number.
#'   \item Layout 4: third panel monitors violations of equivalence theorem vs. iteration number.
#'   \item Layout 5: third panel shows item characteristic curves.
#'   \item Layout 0: only one panel with design.
#'   }
#' @param which   A numeric number which corresponds to a plot for specific block.
#' @param colvec  vector of color sequence for items (default is the R-default black, red, green, etc.)
#'
#' @return An optimal design plot is displayed.
#'
#' @seealso \code{\link{convergenceplot}}
#'
#' @export drawdesign
#' @importFrom graphics abline axis legend lines mtext par points
#'             polygon text box
#' @importFrom grDevices recordPlot dev.off
#'
#' @examples
#'
#' # Example No.1
#' # 2PL-model for three items with parameters (a, b) equal to (1.6, -2), (1.6, 0.5),
#' # and (1.6, 2) for the first, second, and third items, respectively.
#'
#' a<-c(1.6,1.6,1.6); b<-c(-2,0.5,2)
#' ip <- cbind(a,b)
#' yyy <- optical(ip)
#' drawdesign(yyy, ylowl=-1000, refline=0.002)
#'
#' \dontrun{
#'
#' # Example No.2
#' # 2PL-models for six items; the parameters for these items are a=(1.62, 1.4, 0.98, 0.66, 0.92, 0.9),
#' # and b=(-0.47, -1.71, 0.62, -0.15, -1.71, 1.6), respectively.
#'
#' a<-c(1.62, 1.4,0.98,0.66,0.92,0.9); b<-c(-0.47,-1.71, 0.62,-0.15,-1.71,1.6)
#' ip<-cbind(a,b)
#' bid<-c(1,1,1,2,2,2)
#' yyy <- optical(ip,bid=bid,show_progress=2)
#'
#' drawdesign(yyy, ylowl=-1000, refline=0.002, layout=5)
#'
#' }




drawdesign<- function(yyy, ablim=7, ylowl=-9999999, refline=0.002, textout=TRUE,
                      itemnum=NA, layout=1, which=NULL, colvec=1:12)

{

  # Create a list to store plots
  plots<-list()
  B<-length(yyy$ht)

  if(!is.numeric(layout) || layout< 0 || layout> 5)
  {stop("'layout' must be in 0:5")}


  for(i in 1:B){

    rr<- list(dd=yyy$dd[[i]],xi=yyy$xi[[i]], t=yyy$t[[i]], viomax=yyy$viomax[[i]],
              h1=yyy$h1[[i]], ht=yyy$g[[i]], mooiter = yyy$mooiter[[i]],
              time=yyy$time[i,],  oc =yyy$oc[[i]], L=yyy$L, ip=yyy$ip[[i]])

    drawdesign0(rr,ip=rr$ip, ablim, ylowl, refline,textout, itemnum, layout, colvec)
    plots[[i]]<-recordPlot()
    dev.off()
    oldpar <- par(no.readonly = TRUE)
    on.exit( par(oldpar))  # reset graphical parameters
  }



  # Display all plots if 'which' is NULL
  if (is.null(which)) {
    for (i in 1:length(plots)) {
      print(plots[[i]])
      if (length(plots) > 1 && i < length(plots)) {
        cat("Press Enter to continue...")
        invisible(readline())
      }
    }
    return(invisible(plots))  # Return plots invisibly when all are shown
  }


  if (!is.numeric(which) || which < 1 || which > B) {
    stop("'which' must be a number between 1 and the total number of blocks (inclusive).")
  }

  # Display the specific plot
  print(plots[[which]])
  oldpar <- par(no.readonly = TRUE)
  on.exit( par(oldpar))  # reset graphical parameters
  return(invisible(plots))  # Return plots invisibly
}


