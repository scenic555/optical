########################################################################################################################################
# Determine the calibration items to be allocated to an examinee with given
# ability
#  yyy:   design
#  abil:  ability value of the examinee (in relation to a population with
#  standard normal distributed abilities)
# Output:
#  a vector with the item numbers to be allocated to examinee with ability
#  'abil'
########################################################################################################################################
#' Allocate calibration items based on examinee's ability
#'
#' @description
#' This function determine the calibration items to be allocated to an examinee with given
#' ability
#'
#'
#'
#' @param yyy   a \code{\link{optical}} object; the output of a call [optical()]
#' @param abil  ability value of the examinee (relative to a population
#' with abilities following a standard normal distribution)
#'
#' @return A vector containing the item numbers allocated to the examinee with
#' the given ability 'abil'.
#'
#' @export calitems
#'
#' @seealso \code{\link{drawdesign}} , \code{\link{drawdesign_allitems}}
#'
#' @examples
#' # Example No.1
#' # 2PL-models for two items; parameters (a, b)=(1.6, -1) and (1.6, 1), respectively
#' ip <- cbind(c(1.6, 1.6),c(-1, 1))
#' yyy <- optical(ip)
#' calitems(yyy, 0.27)
#'
#' \dontrun{
#' # Example No.2
#' # 2PL-models for six items; parameters a=c(1.62, 0.66, 0.92, 0.90, 0.98, 1.40)
#' # and b=c(-0.47, -0.15, -1.71, 1.60, 0.62, -1.71), respectively.
#' # The calibration of these 6 items with 2PL-models is conducted in two blocks.
#'
#' a<-c(1.62, 0.66,0.92,0.90,0.98,1.40)
#' b<-c(-0.47,-0.15,-1.71,1.60,0.62,-1.71)
#' ip<-cbind(a,b)
#' bid<-c(1,2,2,2,1,1)
#' yyy<-optical(ip,bid=bid)
#' calitems(yyy, 0.27)
#'
#' }




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
