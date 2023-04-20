########################################################################################################################################
# Translates interval boundaries (h1) to new grid and returns design xi
########################################################################################################################################
boundaries2design <- function(t, h1) {
  h  <- h1[, 1]
  I  <- h1[, 3]
  I1 <- h1[, 4]
  xi <- array(data=0, dim=length(t))
  k  <- length(h)

  # find index where condition satisfied
  indx     <- which(t<=h[1])
  xi[indx] <- I[1]
  for (i in 2:k) {
    indx     <- which(t>h[i-1] & t<=h[i])
    xi[indx] <- I[i]
  }
  indx     <- which(t>h[k])
  xi[indx] <- I1[k]
  xi
}
