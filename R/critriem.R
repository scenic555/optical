########################################################################################################################################
# Calculate elements of information matrix at theta for given theta-vector t and k items with
# parameter values in ip. Later we use this to calculate directional derivatives. 2PL or 3PL or mixed 2/3PL
# Using Riemann rule with midpoint for evaluation
########################################################################################################################################
critriem <- function(t, ip) {
  k   <- dim(ip)[1]   # number of items
  mod <- dim(ip)[2]   # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
  n   <- length(t)    # number of thetas in grid
  x   <- (c(t[1], t) + c(t, t[n]))/2    # vector x is t[1], the middle between all t's, and t[n]; length is n+1
  a   <- ip[, 1]
  b   <- ip[, 2]
  if (mod==3) { c <- ip[, 3] }

  if (mod==2 || max(is.na(c)==1)) {
    f11 <- function(x, a, b) { (1/(1+exp(-a*(x-b)))) * (1-(1/(1+exp(-a*(x-b))))) * ((x-b)^2) * dnorm(x) }
    f12 <- function(x, a, b) { (1/(1+exp(-a*(x-b)))) * (1-(1/(1+exp(-a*(x-b))))) * (-a*(x-b)) * dnorm(x) }
    f22 <- function(x, a, b) { (1/(1+exp(-a*(x-b)))) * (1-(1/(1+exp(-a*(x-b))))) * a^2 * dnorm(x) }
  }
  if (mod==3) {
    f   <- function(x, a, b, c) { (c+(1-c)*(1/(1+exp(-a*(x-b))))) * (1-(c+(1-c)*(1/(1+exp(-a*(x-b)))))) * dnorm(x) }
    fA  <- function(x, a, b, c) { (x-b) * ((exp(a*(x-b))) / (c + exp(a*(x-b)))) }
    fB  <- function(x, a, b, c) { (-a*(exp(a*(x-b)))) / (c + exp(a*(x-b))) }
    fC  <- function(x, a, b, c) { (1+(exp(a*(x-b)))) / ((1-c) * (c + exp(a*(x-b)))) }

    a11 <- function(x, a, b, c) { f(x, a, b, c) * (fA(x, a, b, c))^2 }
    a22 <- function(x, a, b, c) { f(x, a, b, c) * (fB(x, a, b, c))^2 }
    a33 <- function(x, a, b, c) { f(x, a, b, c) * (fC(x, a, b, c))^2 }

    a12 <- function(x, a, b, c) { f(x, a, b, c) * fA(x, a, b, c) * fB(x, a, b, c) }
    a13 <- function(x, a, b, c) { f(x, a, b, c) * fA(x, a, b, c) * fC(x, a, b, c) }
    a23 <- function(x, a, b, c) { f(x, a, b, c) * fB(x, a, b, c) * fC(x, a, b, c) }
  }
  if (mod==2) { M <- array(data=NA, dim=c(2, 2, k, n)) }
  if (mod==3) { M <- array(data=NA, dim=c(3, 3, k, n)) }

  for (i in 1:k) {
    for (j in 1:n) {
      if (mod==2 || is.na(c[i])) {
        M11 <- f11((x[j]+x[j+1])/2, a[i], b[i]) * (x[j+1]-x[j])
        M12 <- f12((x[j]+x[j+1])/2, a[i], b[i]) * (x[j+1]-x[j])
        M22 <- f22((x[j]+x[j+1])/2, a[i], b[i]) * (x[j+1]-x[j])
        if (mod==3) { M13 <- M23 <- M33 <- NA }
      }
      if (mod==3 && !is.na(c[i])) {
        M11 <- a11((x[j]+x[j+1])/2, a[i], b[i], c[i]) * (x[j+1]-x[j])
        M12 <- a12((x[j]+x[j+1])/2, a[i], b[i], c[i]) * (x[j+1]-x[j])
        M13 <- a13((x[j]+x[j+1])/2, a[i], b[i], c[i]) * (x[j+1]-x[j])
        M22 <- a22((x[j]+x[j+1])/2, a[i], b[i], c[i]) * (x[j+1]-x[j])
        M23 <- a23((x[j]+x[j+1])/2, a[i], b[i], c[i]) * (x[j+1]-x[j])
        M33 <- a33((x[j]+x[j+1])/2, a[i], b[i], c[i]) * (x[j+1]-x[j])
      }
      if (mod==2) { M[, , i, j] <- matrix(c(M11, M12, M12, M22), 2, 2) }
      if (mod==3) { M[, , i, j] <- matrix(c(M11, M12, M13, M12, M22, M23, M13, M23, M33), 3, 3, byrow=T) }
    }
  }
  M
}
