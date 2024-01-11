################################################################################
#  Convergence plots
#  First panel monitors efficiency of design vs. iteration number
#  Second panel monitors violations of equivalence theorem vs. iteration number
#  Third panel monitors step size used vs. iteration number
#  Maybe add following in future version:
#  Fourth panel shows final grid
#################################################################################
#'  Convergence plot
#'
#'  For each block, convergence plots display efficiency of design, violations
#'  of the equivalence theorem, the step length used vs. iteration number.
#'  These plots are suitable for monitoring the convergence of the optimal item
#'  calibration algorithm.
#'
#'
#' @param yyy   an \code{\link{optical}} object; the output of a call [optical()]
#' @param refline  reference line
#'
#' @details Convergence plots have three panels.
#'  \itemize{
#'  \item {First panel monitors efficiency of design vs. iteration number.}
#'  \item {Second panel monitors violations of equivalence theorem vs. iteration number.}
#'  \item {Third panel monitors step size used vs. iteration number.}
#'   }
#'
#' @return A convergence plot is displayed.
#'
#' @seealso \code{\link{drawdesign}}, \code{\link{drawdesign_allitems}}
#'
#'
#' @export convergenceplot
#' @importFrom graphics abline axis legend lines mtext par points
#'             polygon text box
#' @importFrom grDevices recordPlot
#'
#'
#' @examples
#' # Example No.1
#' # 2PL-model for three items with parameters (a, b) equal to (1.6, -2), (1.6, 0.5),
#' # and (1.6, 2) for the first, second, and third items, respectively.
#'
#' ip <- cbind(c(1.6, 1.6,1.6),c(-2, 0.5,2))
#' yyy <- optical(ip)
#' convergenceplot(yyy, refline=c(0.002, 0.001*0.005/0.45))
#'
#' \dontrun{
#'
#' # Example No.2
#' # 2PL-models for six items; parameters a=(1.62, 1.4, 0.98, 0.66, 0.92, 0.9),
#' # and b=(-0.47, -1.71, 0.62, -0.15, -1.71, 1.6), respectively.
#'
#' a<-c(1.62, 1.4,0.98,0.66,0.92,0.9)
#' b<-c(-0.47,-1.71,0.62,-0.15,-1.71,1.6)
#' ip<-cbind(a,b)
#' bid<-c(1,1,1,2,2,2)
#' yyy <- optical(ip,bid=bid,show_progress=2)
#' convergenceplot(yyy, refline=c(0.002, 0.001*0.005/0.45))
#'
#' }



convergenceplot<- function(yyy, refline=c(0.002, 0.00001)) {


  # Create a list to store plots
  plots<-list()
  B<-length(yyy$ht)

  for(i in 1:B){
    rr<- list(dd=yyy$dd[[i]],xi=yyy$xi[[i]], t=yyy$t[[i]], viomax=yyy$viomax[[i]],
              h1=yyy$h1[[i]], ht=yyy$g[[i]], mooiter = yyy$mooiter[[i]],
              time=yyy$time[i,],  oc =yyy$oc[[i]], L=yyy$L, ip=yyy$ip[[i]])

  convergenceplot0(rr, refline=c(0.002, 0.001*0.005/0.45))
  plots[[i]]<-recordPlot()

  }
  # Display plots one by one using Enter key
  for (i in 1:length(plots)) {
    print(plots[[i]])

    # Check if it's not the last plot and there are more than one plots
    if (length(plots)> 1 && i < length(plots)) {
    cat("Press Enter to continue...")
    invisible(readline())
    }
  }

}





