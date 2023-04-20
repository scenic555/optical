########################################################################################################################################
# Compute interval boundaries
# Structure of result: (interval boundary, probability from last interval
# boundary, item # sampled here, item # sampled next)
# Note that order of the 4 columns was different in earlier versions of the code
########################################################################################################################################
intbounds <- function(xi, t) {
  ibold  <- -100
  n      <- length(t)
  bounds <- c()
  for (j in 2:n) {
    if (xi[j]!=xi[j-1]) {
      ib     <- (t[j]+t[j-1])/2
      bound  <- c(ib, pnorm(ib)-pnorm(ibold), xi[j-1], xi[j])
      bounds <- rbind(bounds, bound)
      ibold  <- ib
    }
  }
  bound  <- c(100, 1-pnorm(ibold), xi[n], xi[n])
  bounds <- rbind(bounds, bound)
  bounds
}
