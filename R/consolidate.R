########################################################################################################################################
# Consolidate design by removing small intervals of size < ex and adding half of them to the left larger and half to the right larger
# interval
########################################################################################################################################
consolidate <- function(h, ex=0.01) {
  hv  <- h[, 1]                          # interval boundaries
  k   <- length(hv)
  dhv <- c(99, hv[2:k]-hv[1:(k-1)])      # differences to last boundaries
  fl  <- (dhv<ex)                        # flag if interval smaller than ex
  nfl <- c(fl[2:k], 0)                   # flag if next interval smaller than ex
  hm  <- cbind(hv, dhv, h[, 3], fl, nfl) # create matrix hm, third column is item to calibrate on interval prior to boundary
  hm1 <- hm[fl==0, ]                     # keep only intervals larger than ex
  hv1 <- hm1[, 1]
  k1  <- length(hv1)
  pv  <- rep(NA, k1)
  hi1 <- hm1[, 3]                        # vector with item numbers to be calibrated
  hn1 <- c(hi1[2:k1], hi1[k1])           # same as before but for the next interval (shifted vector)
  pv[1] <- pnorm(hv1[1])
  for (i in 1:(k1-1)) {
    if (hm1[i, 5]==1) hv1[i] <- (hm1[i, 1] + hm1[i+1, 1] - hm1[i+1, 2])/2  # divide length of deleted intervals into halfs and add to adjacent
    if (i>1) pv[i] <- pnorm(hv1[i]) - pnorm(hv1[i-1])                      # create probability to be in interval
  }
  pv[k1] <- 1-pnorm(hv1[k1-1])
  cbind(hv1, pv, hi1, hn1)
}
