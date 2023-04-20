########################################################################################################################################
# Initial starting design, 1PL, 2PL, 3PL, or mixed 2/3PL model
# Based on asymptotic theorems in Ul Hassan and Miller (2021) on D-optimality and on heuristics
########################################################################################################################################
start.design <- function(t, ip) {
  mod <- dim(ip)[2]   # number of columns in ip (if 2, then 2PL; if 3, then 3PL model or mixture)
  a   <- ip[, 1]
  b   <- ip[, 2]
  if (mod==3) { c <- ip[, 3] }
  borderv <- sort(b, index.return=TRUE)$ix
  xi  <- array(data=0, dim=length(t))
  k   <- length(b)   # number of items
  if (mod==2 & is.na(ip[2, 1])) a <- rep(ip[1, 1], k)

  # find index where these conditions satisfied
  indx1 <- which(t <= -4)
  indx2 <- which(t > -4 & t <= 0)
  indx3 <- which(t > 0  & t <= 4)
  indx4 <- which(t > 4)

  if (mod==2) {
    m <- which(a==min(a))
    # if 2 items have same minimal a, pick the one which has minimal b for low and maximal b for high abilities in case 2PL
    if (length(m)>1) {
      eb  <- which.min(b[m])
      ma  <- m[eb]
      eb1 <- which.max(b[m])
      ma1 <- m[eb1]
    } else {
      ma <- ma1 <- m
    }
    xi[indx1] <- ma
  }
  if (mod==3 && max(is.na(c))==1) {
    # item with less(a) at infinity.
    m  <- which(a==min(a))
    # if 2 items have same minimal a, pick the one which has maximal b for high abilities
    if (length(m)>1) {
      eb1 <- which.max(b[m])
      ma1 <- m[eb1]
    } else {
      ma1 <- m
    }

    # 3pl item with less(b) at -infinity.
    l2  <- which(!is.na(c))
    b2  <- b[l2]
    mc1 <- l2[which.min(b2)]
    xi[indx1] <- mc1
  }
  if (mod==3 && max(is.na(c))==0) {
    m <- which(a==min(a))
    # if 2 items have same minimal a, pick the one which has greater value for (1-c)*exp(a*b) for high abilities in case 3PL
    if (length(m)>1) {
      d   <- (1-c)*exp(a*b)
      ma1 <- which.max(d[m])
    } else {
      ma1 <- which.min(a)
    }
    xi[indx1] <- which.min(b)
  }

  xi[indx2] <- borderv[ceiling((t[indx2]+4)/4*k)]
  xi[indx3] <- borderv[ceiling(t[indx3]/4*k)]
  xi[indx4] <- ma1
  xi
}
