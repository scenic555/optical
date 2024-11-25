########################################################################################################################################
# Function to create an item-type vector out of a item-parameter matrix (ip or ipop)
# Currently, all items can be of Rasch-type (1PL), or all items can be of 2PL or 3PL (including a mixture of them)
# Output:
#  item type as text string ("1PL", "2PL", "3PL") 
########################################################################################################################################
ityfunc <- function(ip){
  k   <- dim(ip)[1]   # number of items
  mod <- dim(ip)[2]   # number of columns in ip (if 2, then 2PL or Rasch-type; if 3, then 3PL model)
  a   <- ip[, 1]
  b   <- ip[, 2]
  c   <- rep(NA, k)
  if (mod==2){
   if (is.na(ip[2, 1])) ity <- rep("1PL", k) else ity <- rep("2PL", k)
  }
  if (mod==3)  ity <- ifelse(is.na(ip[, 3])==T, "2PL", "3PL")
  return(ity)
}
