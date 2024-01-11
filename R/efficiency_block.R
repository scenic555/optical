########################################################################################################################################
# Calculation of efficiency of design xi versus the random design.
# If items = FALSE, single value for total block efficiency
# If items = TRUE, criteria for optimal and random and the efficiency for each item are reported in each column of output;
# last column are then total criteria and efficiency. D-, L-, I-, A-optimality
# xi= vector of a design to be compared with random design
# t=vector of abilities
########################################################################################################################################
#' @title Efficiency of optimal design
#'
#' @description
#' This function computes the efficiency of the D-, I-, and A-optimal designs
#' compared to the random design for each block.
#'
#'
#' @param yyy   a \code{\link{optical}} object; the output of a call [optical()].
#'
#' @param uncert if false (default), abilities are assumed to be known; if true,
#'               handling of uncertainties of Bjermo et al. (2021) is used.
#'
#' @param ipop matrix with item parameters for operational items
#'             (used if uncert=TRUE, only).
#'
#' @param oc optimality criterion: "D" (D-optimality, default),
#'           "I" (I-optimality with standard normal weight function), "A" (A-optimality).
#'
#' @param L     L-matrix (not used for D-optimality).
#'
#' @param items If false (default), only total block efficiency is returned. If true,
#'              for each block, criteria for optimal and random, and the efficiency
#'              for each item are reported in each column of output, except for 1-pl
#'              model where each column represents the parameter efficiency. The last column
#'              shows total criteria and efficiency. D-, L-, I-, A-optimality.
#'
#'
#' @param integ  if true (default), integrate() is used for computation of partial
#'               information matrices; if false, Riemann rule is used.
#'
#' @return       A numerical value is displayed.
#'
#' @seealso \code{\link{optical}}
#'
#' @export efficiency
#'
#' @examples
#'
#' # Example No.1
#' # 2PL-models for two items; parameters (a, b)=(1.6, -1) and (1.6, 1), respectively
#'
#' a<-c(1.6, 1.6); b<-c(-1, 1)
#' ip <- cbind(a,b)
#' yyy <- optical(ip)
#'
#' # Efficiency of A-optimal design compared to random design
#' efficiency(yyy, oc="A")
#'
#' \dontrun{
#'
#' # Example No.2
#' # 2PL-models for six items; the parameters for these items are a=(1.62, 1.4, 0.98, 0.66, 0.92, 0.9),
#' # and b=(-0.47, -1.71, 0.62, -0.15, -1.71, 1.6), respectively.
#'
#' a <- c(1.62, 1.4, 0.98, 0.66, 0.92, 0.9)
#' b <- c(-0.47, -1.71, 0.62, -0.15, -1.71, 1.6)
#' ip <- cbind(a, b)
#' bid <- c(1, 1, 1, 2, 2, 2)
#' yyy <- optical(ip, bid = bid, show_progress = 2)
#'
#' # Efficiency of D-optimal design compared to random design
#' efficiency(yyy, oc = "D")
#'
#' # Efficiency of A-optimal design compared to random design
#' efficiency(yyy, oc = "A")
#'
#' # Efficiency of I-optimal design compared to random design
#' efficiency(yyy, oc = "I")
#' }


efficiency<-function (yyy, uncert = FALSE, ipop, oc = "D", L = NULL,
          items = FALSE, integ = TRUE)
{


B<-length(yyy$ht)
eff<-list()


for(i in 1:B){
rr<- list(dd=yyy$dd[[i]], ip=yyy$ip[[i]], xi=yyy$xi[[i]], t=yyy$t[[i]],viomax=yyy$viomax[[i]],
          h1 =yyy$h1[[i]], ht=yyy$g[[i]], mooiter=yyy$mooiter[[i]],
                 time=yyy$time[i,],
                 oc=yyy$oc, L=yyy$L)

gg<-efficiency0(rr,ip=rr$ip, uncert,ipop, oc=oc,L, items, integ)
eff[[i]]<-gg

}
names(eff)<-paste("block", 1:B,sep="")
eff

}




