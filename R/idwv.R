########################################################################################################################################
# Identify worst violation of equivalence theorem
########################################################################################################################################
idwv <- function(dd, xi) {
  n      <- dim(dd)[2]
  ddmin  <- array(dim=n)
  ddamin <- array(dim=n)
  vio    <- array(dim=n)

  for (j in 1:n) {
    ddmin[j]  <- min(dd[, j])
    ddamin[j] <- which.min(dd[, j])
    vio[j]    <- dd[xi[j], j] - ddmin[j]
  }
  list(ddmin=ddmin, ddamin=ddamin, vio=vio)
}
