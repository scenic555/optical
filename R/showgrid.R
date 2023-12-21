#################################################################################
# Visualize final grid                                                          #
#################################################################################

showgrid <- function(y) {
  k <- length(y$t)

  space <- y$t[2:k] - y$t[1:(k-1)]

  plot(y$t[2:k], space, type="l", xlab="Ability",
       ylab="Space between gridpoints", log="y")

  abline(v=y$h1[, 1], col=2, lty=3)
}
