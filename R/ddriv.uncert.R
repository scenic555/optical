########################################################################################################################################
# Function to calculate directional derivative for design xi.
# Handling of uncertainty in abilities. So far, 2PL, only. D- and L-optimality.
# Operational items can be of Rasch type ("1PL"), 2PL, or 3PL
########################################################################################################################################
ddriv.uncert <- function(M, xi, ip, oc, L=NULL, t, ipop) {
  k   <- dim(ip)[1]                # number of items
  mod <- dim(ip)[2]                # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
  n   <- length(t)                 # number of thetas in grid
  np  <- length(which(!is.na(ip))) # number of parameters
  if (oc=="L") { np <- 0 }         # set first part in dd to 0 for L-optimality
  dd  <- array(dim=c(k, n))
  a   <- ip[, 1]
  b   <- ip[, 2]

  I_inv <- function(ao, bo, x) { #square root of inverse information function
    pq    <- (1/(1+exp(-ao*(x-bo))))*(1-(1/(1+exp(-ao*(x-bo)))))
    I     <- (ao^2 * pq)
    I_inv <- 1/sqrt(sum(I))
    return(I_inv)
  }
  p_tilde <- function(x,a,b,mu,sigma) { # probability of correct response under uncertainty
    (1/(sigma*sqrt(2*pi)))*exp(-0.5*((x-mu)/sigma)^2)*(1/(1+exp(-a*(x-b))))
  }
  eta_1 <- function(x,a,b,theta) {
    ((-(b-x)*exp(-a*(x-b)))/(1 + exp(-a*(x-b)))^2) * dnorm(x, theta, I_inv(ipop, theta))
  }
  eta_2 <- function(x,a,b,theta) {
    ((-a*exp(-a*(x-b)))/(1 + exp(-a*(x-b)))^2) * dnorm(x, theta, I_inv(ipop, theta))
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
        eta1 <- integrate(eta_1, a[i], b[i], t[j], lower = -7, upper = 7)$value
        eta2 <- integrate(eta_2, a[i], b[i], t[j], lower = -7, upper = 7)$value
        p_ <- integrate(p_tilde, a[i], b[i], t[j], I_inv(ipop, t[j]), lower = -7, upper = 7)$value

        dd[i, j] <- np - (1/(p_*(1-p_))) * c(eta1, eta2) %*% Mmid %*% c(eta1, eta2)
        # } else {
        #  fABC     <- c(fA(t[j], a[i], b[i], c[i]), fB(t[j], a[i], b[i], c[i]), fC(t[j], a[i], b[i], c[i]))
        # dd[i, j] <- np - FL(t[j], a[i], b[i], c[i]) * (1-FL(t[j], a[i], b[i], c[i])) * (t(fABC) %*% Mmid %*% fABC)
      }
    }
  }
  dd
}
