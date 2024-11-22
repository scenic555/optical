########################################################################################################################################
# Determine the calibration items to be allocated to an examinee with given ability 
#  yyy:   design
#  abil:  ability value of the examinee (in relation to a population with standard normal distributed abilities)
# Output:
#  a vector with the item numbers to be allocated to examinee with ability 'abil'
########################################################################################################################################
calitems <- function(yyy, abil){
  ci   <- NULL
  for (i in 1:length(yyy$ht)){
    for (j in 1:dim(yyy$ht[[i]])[1]){
      if (yyy$ht[[i]][j, 1] <= abil && abil < yyy$ht[[i]][j, 2]) 
        ci <- c(ci, yyy$ht[[i]][j, 3])
    }
  }
  ci
}
