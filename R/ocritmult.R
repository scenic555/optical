########################################################################################################################################
# Function to calculate optimality critera of multiple designs xii.
# For 1PL, D-optimality. For 2-, 3-PL, or mixed, D- and L-optimality.
########################################################################################################################################
ocritmult <- function(M, xii, oc, L=NULL) {
  h <- dim(xii)[1]      # number of designs compared
  l <- length(dim(M))
  k <- dim(M)[l-1]      # number of items
  critval <- rep(0, h)  # criterion value; for oc="D" it is logdet M^{-1}; for oc="L" it is trace(M^{-1} L)

  for (m in 1:h) {
    if (l==3){
      # 1-PL model
      M0 <- M1 <- M2 <- rep(NA, k)
      for (i in 1:k) {
        M0[i] <- sum(M[1, i, (xii[m, ]==i)])
        M1[i] <- sum(M[2, i, (xii[m, ]==i)])
        M2[i] <- sum(M[3, i, (xii[m, ]==i)])
      }
      schur <- sum(M2 - M1*M1/M0)
      critval[m] <- -log(schur*prod(M0))
    }
    else
    {
      # 2- or 3-PL model or mixed
      for (i in 1:k) {
        MF   <- M[, , i, (xii[m, ]==i)]
        smat <- apply(MF, MARGIN=c(1, 2), sum)
        if (oc=="D") {
          if (dim(smat)[1]>2 && is.na(smat[3, 3])) {
            cvi <- -log(det(smat[1:2, 1:2]))
          } else {
            cvi <- -log(det(smat))
          }
        }
        if (oc=="L") {
          dmat <- diag(rep(1e-99, dim(smat)[1]))
          smat <- smat+dmat  # to avoid error when smat is 0-matrix; so M^{-1} becomes very large and this m is not chosen
          if (dim(smat)[1]>2 && is.na(smat[3, 3])) {
            cvi <- sum(diag(solve(smat[1:2, 1:2]) %*% L[1:2, 1:2, i]))
          } else {
            cvi <- sum(diag(solve(smat) %*% L[, , i]))
          }
        }
        critval[m] <- critval[m] + cvi
      }
    }
  }
  critval
}
