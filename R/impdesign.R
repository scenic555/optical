########################################################################################################################################
# Improve design
########################################################################################################################################
impdesign <- function(viomax, vio, imf, xii, xi, ddamin, dd, t, k) {
  li <- length(imf)
  n  <- length(xi)

  for (m in 1:li) {
    for (j in 1:n) {
      if (vio[j]>(1-imf[m])*viomax) {
        # change sampling to other item
        xii[m, j] <- ddamin[j]
      } else {
        xii[m, j] <- xi[j]
      }
    }
  }
  list(xii=xii)
}
