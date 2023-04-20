########################################################################################################################################
# Inner loop for optimization (results for interval bounds), using functions above
# oc     = optimality criterion (can be "D" or "L"; in the latter case, also an L-matrix is required)
# xi     = starting design
# sss    = step size stopping criterion
# falpha = factor alpha for adjusting the step size vector
# sdr    = stop if design repeated (flag TRUE/FALSE)
########################################################################################################################################
innerloop <- function(t, ip, oc="D", L=NULL, uncert=FALSE, ipop, imf, maxiter=1000, eps=0.001, sss=0.0001, falpha=1.08, sdr=TRUE, integ=TRUE, xi) {
  n   <- length(t)      # number of different ability levels (grid size)
  k   <- dim(ip)[1]     # number of items
  mod <- dim(ip)[2]     # number of columns in ip (if 2, then 2PL; if 3, then 3PL model)
  np  <- length(which(!is.na(ip))) # number of parameters
  li  <- length(imf)    # number of different designs compared in parallel (K in paper)

  xii <- array(dim=c(li, n)) # matrix li*n
  xii[1, ] <- xi

  # calculate elements of matrix at each ability point: required to calculate directional derivative and criterion log(det(M^-1))
  if (uncert) M <- crit.uncert(t, ip, a_op=ipop[, 1], b_op=ipop[, 2])
  else {
    if (integ) M <- crit(t, ip) else M <- critriem(t, ip)
  }
  critvalRand <- ocritr(M, oc=oc, L=L)  # criterion of random design

  iterc  <- 0           # iteration counter
  viomax <- 999999      # initialisation of largest violation of equivalence theorem
  circle <- FALSE       # flag if iteration went into a loop (circular)
  effi   <- c()         # vector for efficiencies in each iteration
  lvio   <- c()         # vector of largest violations of equivalence theorem
  vss    <- c()         # vector of step-lengths used
  vssmin <- c()
  vssmax <- c()
  while (viomax>eps & iterc<maxiter & imf[li]>sss & circle==FALSE) {
    iterc <- iterc + 1

    critval <- ocritmult(M, xii, oc=oc, L=L) # calculate criterion value (log(det(M^-1)) for D-opt) for given position of item on theta
    mm <- which.min(critval)

    if (iterc>1 && mm==1)                  imf <- imf/falpha   # reduce step sizes if smallest step size was used
    if (iterc>1 && mm==li && imf[li]<0.5)  imf <- imf*falpha   # increase step sizes if largest step size was used

    # calculation of directional derivative
    xi <- xii[mm, ]
    if (uncert)  dd <- ddriv.uncert(M, xi, ip, oc=oc, L=L, t, a_op=ipop[, 1], b_op=ipop[, 2])
    else       dd <- ddriv(M, xi, ip, oc=oc, L=L, t)

    tt <- idwv(dd, xi)
    # identification of violation of equivalence theorem
    ddmin  <- tt$ddmin
    ddamin <- tt$ddamin
    vio    <- tt$vio

    viomax <- max(vio)
    if (oc=="D") { effi <- c(effi, exp(critvalRand-ocrit(M, xi, oc=oc))^(1/np)) }
    if (oc=="L") { effi <- c(effi, (critvalRand/ocrit(M, xi, oc=oc, L=L))^(1)) }   # should it be exponent 1/k instead of 1 ???
    lvio   <- c(lvio, viomax)
    vss    <- c(vss, imf[mm])
    vssmin <- c(vssmin, imf[1])
    vssmax <- c(vssmax, imf[li])
    print(paste(iterc, " crit.val.=", round(critval[mm], 5), " MV=", round(viomax, 7), " SS=", round(imf[mm], 7)))

    if (sdr==TRUE) {
      if (iterc>16 && min(xi==xiold)==1) { circle <- TRUE }    # if design is the same as saved in last of iteration 8, 16, 24, ... then stop
      if (iterc %% 8 == 0) { xiold <- xi }                     # save results every 8th iteration to check later circulation
    }

    idd  <- impdesign(viomax, vio, imf, xii, xi, ddamin, dd, t, k)     # improve design
    xii  <- idd$xii
  }
  # Matrix to monitor iterations
  moiter <- cbind(1:length(effi), effi, lvio, vss, vssmin, vssmax)

  list(dd=dd, xi=xi, viomax=viomax, moiter=moiter)
}
