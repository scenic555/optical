########################################################################################################################################
# Calculate elements of information matrix at theta for given theta-vector t and k items with
# parameter values in ip. Later we use this to calculate directional derivatives.
# Handling of uncertainty in abilities. So far 2PL, only.
########################################################################################################################################
crit.uncert <- function(t, ip, a_op, b_op) {
  k   <- dim(ip)[1]   # number of items
  mod <- dim(ip)[2]   # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
  n   <- length(t)    # number of thetas in grid
  x   <- (c(t[1], t) + c(t, t[n]))/2    # vector x is t[1], the middle between all t's, and t[n]; length is n+1
  a   <- ip[, 1]
  b   <- ip[, 2]
  # if (mod==3) { c <- ip[, 3] }
  if (mod==2 || max(is.na(c)==1)) {
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
      ((-(b-x)*exp(-a*(x-b)))/(1 + exp(-a*(x-b)))^2)*dnorm(x,theta,I_inv(a_op,b_op,theta))
    }
    eta_2 <- function(x,a,b,theta) {
      ((-a*exp(-a*(x-b)))/(1 + exp(-a*(x-b)))^2)*dnorm(x,theta,I_inv(a_op,b_op,theta))
    }
  }

  if (mod==2) { M <- array(data=NA, dim=c(2, 2, k, n)) }
  # if (mod==3) { M <- array(data=NA, dim=c(3, 3, k, n)) }

  for (i in 1:k) {
    for (j in 1:n) {
      if (mod==2 || is.na(c[i])) {
        ieta1 <- integrate(eta_1, a[i], b[i], t[j], lower = -7, upper = 7)$value
        ieta2 <- integrate(eta_2, a[i], b[i], t[j], lower = -7, upper = 7)$value
        eta11 <- ieta1^2
        eta12 <- ieta1 * ieta2
        eta22 <- ieta2^2
        p_    <- integrate(p_tilde, a[i], b[i], t[j], I_inv(a_op, b_op, t[j]), lower = -7, upper = 7)$value

        h    <- dnorm(t[j])
        step <- x[j+1]-x[j]

        M11 <- 1/(p_*(1-p_)) * eta11*h*step
        M12 <- 1/(p_*(1-p_)) * eta12*h*step
        M22 <- 1/(p_*(1-p_)) * eta22*h*step

        if (mod==3) { M13 <- M23 <- M33 <- NA }
      }

      if (mod==2) { M[, , i, j] <- matrix(c(M11, M12, M12, M22), 2, 2) }
      #     if (mod==3) { M[, , i, j] <- matrix(c(M11, M12, M13, M12, M22, M23, M13, M23, M33), 3, 3, byrow=T) }
    }
  }
  M
}

