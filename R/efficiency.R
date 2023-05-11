########################################################################################################################################
# Calculation of efficiency of design xi versus the random design.
# If items = FALSE, single value for total block efficiency
# If items = TRUE, criteria for optimal and random and the efficiency for each item are reported in each column of output;
# last column are then total criteria and efficiency. D-, L-, I-, A-optimality
#  xi= vector of a design to be compared with random design
#  t=vector of abilities
########################################################################################################################################

#' @title Efficiency of optimal design
#'
#' @description
#' This function computes the efficiency of the D, I, and A optimal designs
#' compared to the random design.
#'
#'
#' @param yyy   a \code{\link{optical}} object; the output of a call [optical()]
#' @param ip  matrix with item parameters for all items (number of rows determines number of items;
#'            number of columns is 2 (for 2PL; or 1PL with common a-parameter when NA in first column from second item)
#'            or 3 (for 3PL; or mixed 2/3-PL with NA for 2PL-items in third column)
#' @param uncert if false (default), abilities are assumed to be known; if true,
#'  handling of uncertainties of Bjermo et al. (2021) is used.
#' @param ipop matrix with item parameters for operational items
#' (used if uncert=TRUE, only).
#' @param oc optimality criterion: "D" (D-optimality, default),
#' "I" (I-optimality with standard normal weight function), "A" (A-optimality).
#' @param L L-matrix (not used for D-optimality)
#' @param items if false (default), only total block efficiency is returned; if true,
#' criteria for optimal and random and the efficiency
#' for each item are reported in each column of output. Last column are then
#' total criteria and efficiency. D-, L-, I-, A-optimality
#' @param integ if true (default), integrate() is used for computation of partial
#'  information matrices; if false, Riemann rule is used.
#'
#' @seealso \code{\link{optical}}
#'
#' @export efficiency
#'
#' @examples
#' # 2PL-models for two items; parameters (a, b)=(1.6, -1) and (1.6, 1), respectively
#' ip <- cbind(c(1.6, 1.6),c(-1, 1))
#'
#' yyy <- optical(ip, oc="D", uncert=FALSE, ipop,
#'                imf=c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.45),
#                 maxiter=rep(300, 6),eps=rep(0.002, 6),
#'                nnn=c(0, 50, 50, 200, 200, 200),
#'                nsp=c(0.001, 0.0001, 0.0001, 0.00001, 0.00001, 0.00001),
#'                sss=0.001, falpha=1.08, ig=3, ex=0)
#'
#' # Efficiency of D-optimal design compared to random design
#' efficiency(yyy, ip, oc="D")
#'
#' # Efficiency of I-optimal design compared to random design
#' efficiency(yyy, ip, oc="I")
#'
#' # Efficiency of A-optimal design compared to random design
#' efficiency(yyy, ip, oc="A")





efficiency <- function(yyy, ip, uncert=FALSE, ipop, oc="D", L=NULL, items=FALSE,
                       integ=TRUE) {

  t<-yyy$t; xi<-yyy$xi

  if (items==FALSE) {
    np <- sum(!is.na(ip))        # number of parameters for whole model
  } else {
    np <- rowSums(!is.na(ip))    # vector with number of parameters per item
  }
  oc2 <- oc
  # calculate elements of matrix at each ability point
  if (uncert) M <- crit.uncert(t, ip, a_op=ipop[, 1], b_op=ipop[, 2])
  else {
    if (integ) M <- crit(t, ip) else M <- critriem(t, ip)
  }
  if (oc=="A" || oc=="I") {
    L   <- lmatrix(ip, oc)
    oc2 <- "L"
  }
  crit_od   <- ocrit(M, xi, oc2, L, items)
  crit_rand <- ocritr(M, oc2, L, items)
  if (oc2=="D") {
    eff <- exp(crit_rand - crit_od)^(1/np)
  }
  if (oc2=="L") {
    eff <- (crit_rand / crit_od)^(1)
  }
  # compute total criterion values and total efficiency
  if (items==TRUE) {
    eff <- rbind(crit_od, crit_rand, eff)
    tot <- rowSums(eff)
    if (oc2=="D") { tot[3] <- exp(tot[2] - tot[1])^(1/sum(np)) } else { tot[3] <- tot[2] / tot[1] }
    eff <- cbind(eff, tot)
  }
  eff
}
