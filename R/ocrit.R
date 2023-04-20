########################################################################################################################################
# Function to calculate optimality criterion of design xi.
# If items = TRUE, criteria for each item are reported (in case of 1Pl, for each parameter a, b_i).
# For 1PL, D-optimality. For 2-, 3-PL, or mixed, D- and L-optimality.
########################################################################################################################################
ocrit <- function(M, xi, oc, L=NULL, items=FALSE) {
  l <- length(dim(M))
  k <- dim(M)[l-1]    # number of items
  # criterion value; for oc="D" it is logdet M^{-1}; for oc="L" it is trace(M^{-1} L); either sum or all are reported for items
  if (items==TRUE) { critval <- c() } else { critval <- 0 }
  if (l==3){
    # 1-PL model
    M0 <- M1 <- M2 <- rep(NA, k)
    for (i in 1:k) {
      M0[i] <- sum(M[1, i, (xi==i)])
      M1[i] <- sum(M[2, i, (xi==i)])
      M2[i] <- sum(M[3, i, (xi==i)])
    }
    schur <- sum(M2 - M1*M1/M0)
    if (items==TRUE) { critval <- c(1/schur, (M0+M1*M1/schur)/(M0*M0)) } else { critval <- -log(schur*prod(M0)) }
  }
  else
  {
    # 2- or 3-PL model or mixed
    for (i in 1:k) {
      MF   <- M[, , i, (xi==i)]
      smat <- apply(MF, MARGIN=c(1, 2), sum)
      if (oc=="D") {
        if (dim(smat)[1]>2 && is.na(smat[3, 3])) {
          cvi <- -log(det(smat[1:2, 1:2]))
        } else {
          cvi <- -log(det(smat))
        }
      }
      if (oc=="L") {
        if (dim(smat)[1]>2 && is.na(smat[3, 3])) {
          cvi <- sum(diag(solve(smat[1:2, 1:2]) %*% L[1:2, 1:2, i]))
        } else {
          cvi <- sum(diag(solve(smat) %*% L[, , i]))
        }
      }
      if (items==TRUE) { critval <- c(critval, cvi) } else { critval <- critval + cvi }
    }
  }
  critval
}
