########################################################################################################################################
# Calculation of efficiency of design xi versus the random design.
# If items = TRUE, criteria for optimal and random and the efficiency for each item are reported in each column of output;
# last column are then total criteria and efficiency. D-, L-, I-, A-optimality
########################################################################################################################################
efficiency <- function(t, ip, uncert=FALSE, ipop, xi, oc, L=NULL, items=FALSE, integ=TRUE) {
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
    eff <- (crit_rand / crit_od)^(1)  # should it be exponent 1/k instead of 1 ???
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
