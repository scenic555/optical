########################################################################################################################################
# Adapt grid: at all interval boundaries, place nnn new theta values to the left and to the right with spacing nsp
# nnn = number new nodes
# nsp = node spacing
# ig  = inner grid between -ig and ig
########################################################################################################################################
adaptgrid <- function(t, y, nnn=10, nsp=0.0001, ig=3) {
  n  <- length(t)
  dd <- y$dd
  xi <- y$xi
  vi <- idwv(dd, xi)$vio    # Violations

  tn <- t  # new grid
  # add points around interval boundaries if there is a violation of the equivalence theorem, spacing multiplied with 10 if ability < -ig or >ig
  for (i in 2:(n-1)) {
    if (vi[i]>0 && (xi[i]!=xi[i-1] || xi[i]!=xi[i+1])) {
      if (abs(t[i])>ig) {
        tn <- c(tn, seq(t[i], t[i]-nnn*nsp*10, by=-nsp*10), seq(t[i], t[i]+nnn*nsp*10, by=nsp*10))
      } else {
        tn <- c(tn, seq(t[i], t[i]-nnn*nsp, by=-nsp), seq(t[i], t[i]+nnn*nsp, by=nsp))
      }
    }
  }
  unique(round(sort(tn), 10))
}
