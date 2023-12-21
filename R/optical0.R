########################################################################################
#                         Optimal item calibration                                     #
#   Optimal item calibration works only for a block of items numbering between 3 and 5 #
#                                                                                      #
#   Calibrate items following a 2PL, 3PL, mixture of 2PL and 3PL                       #
#   model, or 2PL with common discrimination for all items (Rasch-type).               #
########################################################################################

#  ip:          matrix with item parameters for all items (number of rows determines
#               number of items; number of column is 2 (2PL or Rasch-type with NA from second
#               item in first column) or 3 (3PL or mixed 2/3-PL with NA for
#               2PL-items in third column).

#  oc:          optimality criterion: "D" (D-optimality, default),
#               "I" (I-optimality with standard normal weight function), "A" (A-optimality).

#  uncert:      if false (default), abilities are assumed to be known; if true,
#               handling of uncertainties of Bjermo et al. (2021) is used.

#  ipop:        matrix with item parameters for operational items
#               (used if uncert=TRUE, only).

#  imf:         the vector of step-lengths; default
#               c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.45).

#  maxiter:     maximal number of iterations in each inner loop, the length
#               of this vector defines the number of outer loops.

#  eps:         convergence criterion (maximum violation of eq.th.), vector with
#               value for each iteration in the outer loop, but the same number for all
#               iterations is recommended.

#  nnn:         number of new nodes added at each position in the adaptive grid,
#               vector with value for each iteration in the outer loop (nnn \[1] not used).

#  nsp:         node spacing between new nodes, vector with value for each iteration
#               in the outer loop (nsp \[1] is the spacing between nodes of the starting grid).

#  sss:         step size stopping criterion.
#  falpha:      factor alpha for adjusting the step size vector (should be > 1).
#  sdr:         stop if design repeated (flag TRUE/FALSE).
#  ig:          inner grid between -ig and ig.

#  ex:          intervals of size < ex will be removed (consolidate);
#               if ex=0, no consolidation will be done.

#  integ:       if true (default), integrate() is used for computation of partial
#               information matrices; if false, Riemann rule is used.
#
#  show_progress:    If 1 (default), no output will be printed for each iteration.
#                    If 2, the + symbols will be printed on a line for each Iteration
#                    If 3, some output of the function will be printed.




#   an object of class "optical0" is returned, which is a list with
#   following instances:

#   dd:            directional derivatives of optimal solution
#   xi:            optimal solution.
#   t:             final grid of ability values which was used.
#   viomax:        largest violation of eq.th. from final solution (if < eps, alg.
#                  has converged, otherwise not)
#   h1:            interval boundaries for optimal solution
#   ht:            Refined table of interval boundaries for optimal design with
#                  calibrated items and their corresponding probabilities}
#   mooiter:       monitoring iterations; information about each iteration to produce
#                  convergence plots.
#   time:          running time of algorithm in minutes.
#   oc:            optimality criterion ("D", "I", "A", "L").
#   L:             L-matrix (not for D-optimality).
#
#

#   Eexamples:
#
#       2PL-models for two items; parameters (a, b)=(1.6, -1) and (1.6, 1), respectively
#
#       ip <- cbind(c(1.6, 1.6),c(-1, 1))
#       yyy <- optical0(ip)
#       yyy
#
#
#      1PL-models with common discrimination parameter for two items
#      (model assumption is that both have same discrimination);
#      parameters (a, b)=(1.6, -1) and (1.6, 1), respectively;
#      NA for discrimination means that item has same parameter as preceeding item
#
#      ip <- cbind(c(1.6, NA), c(-1, 1))
#      yyy <- optical(ip)
#      yyy
#
#      3PL-models for three items; parameters (a, b, c)=(1, 2, 2.5),
#      (-1.5, 0.5, 2) and (0.2, 0.1, 0.05), respectively.
#
#      ip <- cbind(c(1, 2, 2.5),c(-1.5, 0.5, 2),c(0.2, 0.1, 0.05))
#      yyy <- optical(ip)
#      yyy




optical0<- function(ip, oc="D", uncert=FALSE, ipop,
                    imf=c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.45), maxiter=rep(300, 6), eps=rep(0.002, 6),
                    nnn=c(0, 50, 50, 200, 200, 200), nsp=c(0.001, 0.0001, 0.0001, 0.00001, 0.00001, 0.00001),
                    sss=0.001, falpha=1.08, sdr=TRUE, ig=3, ex=0, integ=TRUE,
                    show_progress=1) {

  starttime <- proc.time()
  L   <- NULL
  oc2 <- oc
  # For I- and A-optimality, compute L-matrix and solve then L-optimality
  if (oc=="I" || oc=="A") {
    L   <- lmatrix(ip, oc)
    oc2 <- "L"
  }
  # starting grid with node spacing nsp[1] between -ig and ig and spacing 10*nsp[1] between -7 and -ig and between ig and 7
  t   <- c(seq(-ig, -7, by=-nsp[1]*10), seq(0, -ig, by=-nsp[1]),
           seq(0, ig, by=nsp[1]), seq(ig, 7, by=nsp[1]*10))
  t   <- unique(sort(t))
  xis <- start.design(t, ip)                  # starting design

  oitermax  <- min(length(maxiter), length(eps), length(nnn), length(nsp))     # total number of outer iterations
  oiterc    <- 1                                                               # counter for outer iterations
  if(show_progress!=1){
  message("-----> Outer iteration =", oiterc)
  }
  # Run optimization (maxiter = maximum number of iterations, eps = stopping criterion for maximum violation of equivalence criterion)
  yy  <- innerloop(t, ip, oc=oc2, L=L, uncert=uncert, ipop=ipop, imf,
                   maxiter=maxiter[oiterc], eps=eps[oiterc], sss=sss,
                   falpha=falpha, sdr=sdr, xi=xis, integ=integ,
                   show_progress=show_progress)
  h1  <- intbounds(yy$xi, t)                  # Create boundaries for theta values
  if (ex>0) h1 <- consolidate(h1, ex=ex)
  mooiter <- cbind(oiterc, yy$moiter)         # Monitor outer (and inner) iterations
  while (oiterc<oitermax) {
    oiterc <- oiterc+1
    if (yy$viomax>eps[oiterc]) {
      # Adapt grid automatically
      if(show_progress!=1){
       message("-----> Adapt grid; outer iteration =", oiterc)
      }
      t   <- adaptgrid(t, yy, nnn=nnn[oiterc], nsp=nsp[oiterc], ig=ig)
      xis <- boundaries2design(t, h1)
      # Run optimization
      yy  <- innerloop(t, ip, oc=oc2, L=L, uncert=uncert, ipop=ipop, imf,
                       maxiter=maxiter[oiterc], eps=eps[oiterc], sss=sss,
                       falpha=falpha, sdr=sdr, xi=xis, integ=integ,
                       show_progress=show_progress)
      h1  <- intbounds(yy$xi, t)              # Create boundaries for theta values
      if (ex>0) h1 <- consolidate(h1, ex=ex)
      mooiter <- rbind(mooiter, cbind(oiterc, yy$moiter))
    }
  }
  if (yy$viomax>eps[oiterc])
  { warning(paste0("Failed to converge. Maximum violation of eq.th.=",
                   round(yy$viomax,5), " instead of ", eps[oiterc], ".")) }
  # Time in minutes
  runtime <- (proc.time()-starttime)/60

  # Refine table for optimal design
  r<-nrow(h1)
  bon<-c(-Inf,h1[(1:(r-1)),1],Inf)
  Lower<-bon[1:(length(bon)-1)]
  Upper<-bon[2:length(bon)]
  Item<-h1[,3]
  Probability<-h1[,2]
  g<-cbind(Lower,Upper,Item,Probability)
  rownames(g)<- 1:length(Lower)
  itemnam<- as.numeric(rownames(ip))
  g[, 3] <- itemnam[g[, 3]]
  #h1[, 3] <- itemnam[h1[, 3]]
  #h1[, 4] <- itemnam[h1[, 4]]


  output<-list(dd=yy$dd, xi=yy$xi, t=t, viomax=yy$viomax, h1=h1,ht=g, mooiter=mooiter,
       time=runtime, oc=oc, L=L)

  #class(output)<-"optical0"

  #print.optical0(output)

  # Return the output
  return(output)

}






# print.optical0 <- function(x, digits = max(3, getOption("digits") - 3), ...) {
#   row <- paste(rep("=", 67), collapse = "")
#   cat(row, "\n")
#   cat("Table of interval boundaries for", paste0(x$oc, "-optimal design with items and\n",
#                                                  "probabilities (expected proportion of examinees in this interval)"), "\n")
#   cat(row, "\n")
#
#   print(x$ht, digits, ...)
# }
