########################################################################################################################################
# Function to calculate directional derivative for design xi, 2PL or 3PL or mixed 2/3PL. D- and L-optimality. Also 1PL but D-optimality, only.
########################################################################################################################################
ddriv <- function(M, xi, ip, oc, L=NULL, t) {
  k   <- dim(ip)[1]                # number of items
  mod <- dim(ip)[2]                # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
  if (mod==2 & is.na(ip[2, 1])) mod <- 1
  n   <- length(t)                 # number of thetas in grid
  np  <- length(which(!is.na(ip))) # number of parameters
  if (oc=="L") { np <- 0 }         # set first part in dd to 0 for L-optimality
  dd  <- array(dim=c(k, n))
  a   <- ip[, 1]
  b   <- ip[, 2]
  if (mod==1) {
    M0 <- M1 <- M2 <- rep(NA, k)
    for (i in 1:k) {
      M0[i] <- sum(M[1, i, (xi==i)])
      M1[i] <- sum(M[2, i, (xi==i)])
      M2[i] <- sum(M[3, i, (xi==i)])
    }
    schur <- sum(M2 - M1*M1/M0)
    for (i in 1:k) {
      for (j in 1:n) {
        # directional derivatives dd[i, j] for item i at theta_j
        #dd[i, j] <- np - 1/(1+exp(-a[1]*(t[j]-b[i]))) * (1-1/(1+exp(-a[1]*(t[j]-b[i])))) * ((t[j]-b[i])^2/schur-2*(t[j]-b[i])*M1[i]/(schur*M0[i])+(M0[i]+M1[i]^2/schur)/M0[i]^2)
        dd[i, j] <- np - 1/schur * 1/(1+exp(-a[1]*(t[j]-b[i]))) * (1-1/(1+exp(-a[1]*(t[j]-b[i])))) * ((t[j]-b[i]-M1[i]/M0[i])^2+schur/M0[i])
      }
    }
  }
  else
  {
    if (mod==3) {
      c <- ip[, 3]
      # derivatives for 3PL model
      fA <- function(x, a, b, c) { (x-b) * ((exp(a*(x-b))) / (c + exp(a*(x-b)))) }
      fB <- function(x, a, b, c) { (-a*(exp(a*(x-b)))) / (c + exp(a*(x-b))) }
      fC <- function(x, a, b, c) { (1+(exp(a*(x-b)))) / ((1-c)*(c + exp(a*(x-b)))) }
      FL <- function(x, a, b, c) { (c + ((1 - c) / (1 + exp(-a*(x-b))))) }
    }
    for (i in 1:k) {
      if (mod==3 && is.na(c[i])) { ar1 <- M[1:2, 1:2, i, (xi==i)] } else { ar1 <- M[, , i, (xi==i)] }
      Minv <- solve(apply(ar1, MARGIN=c(1, 2), sum))
      if (oc=="D") { Mmid <- Minv }
      if (oc=="L") {
        if (mod==3 && is.na(c[i])) {
          Mmid <- Minv %*% L[1:2, 1:2, i] %*% Minv
        } else {
          Mmid <- Minv %*% L[, , i] %*% Minv
        }
      }
      for (j in 1:n) {
        # directional derivatives dd[i, j] for item i at theta_j
        if (mod==2 || is.na(c[i])) {
          dd[i, j] <- np - 1/(1+exp(-a[i]*(t[j]-b[i]))) * (1-1/(1+exp(-a[i]*(t[j]-b[i])))) * c(t[j]-b[i], -a[i]) %*% Mmid %*% c(t[j]-b[i], -a[i])
        } else {
          fABC     <- c(fA(t[j], a[i], b[i], c[i]), fB(t[j], a[i], b[i], c[i]), fC(t[j], a[i], b[i], c[i]))
          dd[i, j] <- np - FL(t[j], a[i], b[i], c[i]) * (1-FL(t[j], a[i], b[i], c[i])) * (t(fABC) %*% Mmid %*% fABC)
        }
      }
    }
  }
  dd
}
