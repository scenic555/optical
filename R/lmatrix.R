########################################################################################################################################
# Computation of L-matrix for I- and A-optimality
########################################################################################################################################
lmatrix <- function(ip, oc) {
  # L-matrix computation for I-optimality
  if (oc=="I") {
    k   <- dim(ip)[1]   # number of items
    mod <- dim(ip)[2]   # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
    a   <- ip[, 1]
    b   <- ip[, 2]
    if (mod==3) { c <- ip[, 3] }
    if (mod==2) {
      Lx  <- rbind(1+b^2, a*b, a*b, a^2)
      L   <- array(Lx, c(2, 2, k))
    }
    if (mod==3) {
      fA  <- function(x, a, b, c) { (x-b) * ((exp(a*(x-b))) / (c + exp(a*(x-b)))) }
      fB  <- function(x, a, b, c) { (-a*(exp(a*(x-b)))) / (c + exp(a*(x-b))) }
      fC  <- function(x, a, b, c) { (1+(exp(a*(x-b)))) / ((1-c)*(c + exp(a*(x-b)))) }

      a11 <- function(x, a, b, c) { dnorm(x) * (fA(x,a,b,c))^2 }
      a22 <- function(x, a, b, c) { dnorm(x) * (fB(x,a,b,c))^2 }
      a33 <- function(x, a, b, c) { dnorm(x) * (fC(x,a,b,c))^2 }
      a12 <- function(x, a, b, c) { dnorm(x) * fA(x,a,b,c) * fB(x,a,b,c) }
      a13 <- function(x, a, b, c) { dnorm(x) * fA(x,a,b,c) * fC(x,a,b,c) }
      a23 <- function(x, a, b, c) { dnorm(x) * fB(x,a,b,c) * fC(x,a,b,c) }

      L   <- array(data=NA, dim=c(3, 3, k))

      for (i in 1:k) {
        if (is.na(c[i])) {
          L[, , i] <- c(1+b[i]^2, a[i]*b[i], NA, a[i]*b[i], a[i]^2, rep(NA, 4))
        }
        if (!is.na(c[i])) {
          L11      <- integrate(a11, a[i], b[i], c[i], lower=-7, upper=7)$value
          L12      <- integrate(a12, a[i], b[i], c[i], lower=-7, upper=7)$value
          L13      <- integrate(a13, a[i], b[i], c[i], lower=-7, upper=7)$value
          L22      <- integrate(a22, a[i], b[i], c[i], lower=-7, upper=7)$value
          L23      <- integrate(a23, a[i], b[i], c[i], lower=-7, upper=7)$value
          L33      <- integrate(a33, a[i], b[i], c[i], lower=-7, upper=7)$value
          L[, , i] <- matrix(c(L11, L12, L13, L12, L22, L23, L13, L23, L33), 3, 3, byrow=T)
        }
      }
    }
  }
  # L-matrix computation for A-optimality
  if (oc=="A") {
    k   <- dim(ip)[1]   # number of items
    mod <- dim(ip)[2]   # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
    L   <- array(diag(mod)/k, c(mod, mod, length(ip[, 1])))   # L=identity matrix - division by k has to be investigated further
  }
  L
}
