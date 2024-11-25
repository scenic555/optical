########################################################################################################################################
# Function to calculate test information about abilities from operational items
# ipop = item parameter matrix of operational items
# x    = information will be calculated at theta=x
# Output
#   square root of inverse information function
########################################################################################################################################
I_inv <- function(ipop, x) { 
  i1pl <- i2pl <- i3pl <- 0
  ity   <- ityfunc(ipop)
  ity1  <- (ity=="1PL")
  ity2  <- (ity=="2PL")
  ity3  <- (ity=="3PL")
  if (sum(ity1)>0){
    ao    <- ipop[ity1, 1]
    aa    <- NA
    for (i in 1:length(ao)){
      if (is.na(ao[i])==F) aa <- ipop[i, 1]
      if (is.na(ao[i])==T) ao[i] <- aa
    }
    bo    <- ipop[ity1, 2]
    pq    <- (1/(1+exp(-ao*(x-bo))))*(1-(1/(1+exp(-ao*(x-bo)))))
    i1pl  <- (ao^2 * pq)
  }
  if (sum(ity2)>0){
    ao    <- ipop[ity2, 1]
    bo    <- ipop[ity2, 2]
    pq    <- (1/(1+exp(-ao*(x-bo))))*(1-(1/(1+exp(-ao*(x-bo)))))
    i2pl  <- (ao^2 * pq)
  }
  if (sum(ity3)>0){
    ao    <- ipop[ity3, 1]
    bo    <- ipop[ity3, 2]
    co    <- ipop[ity3, 3]
    po    <- (co + (1-co)/(1+exp(-ao*(x-bo))))
    i3pl  <- (ao^2 * (1-po)*(po-co)^2/(po*(1-co)^2))
  }
  I_inv <- 1/sqrt(sum(i1pl)+sum(i2pl)+sum(i3pl))
  return(I_inv)
}
