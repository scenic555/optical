#' @title Optimal item calibration
#'
#' @description
#' Calibrate items using a 2PL, 3PL, a mixture of 2PL and 3PL models,
#' or a 2PL model with a common discrimination parameter for all items (Rasch-type).
#' This versatile function is designed to address calibration challenges involving
#' a substantial number of items (100-200) by grouping them into blocks of 3 to 5 items each.
#'
#'
#' @param ip Matrix containing item parameters for all items. The number of rows
#' corresponds to the number of items, and columns represent item parameters.
#' There are four possible configurations:
#'
#' \itemize{
#' \item{1. 2PL or Rasch-type:}{ The first two columns contain item parameters,
#' while the first column contains 'NA' from the second item}
#' \item{2. 2PL items:}{ First two columns contain item parameters}
#' \item{3. 3PL items:}{ All three columns contain item parameters.}
#' \item{4. Mixed 2/3-PL:}{ For 3-PL items, all three columns contain item parameters,
#' while 2-PL items have NA in the third column.}
#' }
#'
#' @param bid Vector of integers indicating the block assignments for items.
#' The length of bid should match the number of rows in the 'ip' matrix.
#' Each element in the vector specifies which item belongs to which block.
#' bid is utilized in the calibration process when dealing with more than 5 items.
#' Note that the block should exclusively consist of Rasch-type items, 2-PL items,
#' 3-PL items, or a combination of 2-3PL items. If blocks consist of Rasch-type,
#' 2-PL, and 3PL items, then we need to introduce a third column with 'NA'
#' for 2-PL and Rasch-type items in ip matrix.
#'
#' @param oc optimality criterion: "D" (D-optimality, default),
#' "I" (I-optimality with standard normal weight function), "A" (A-optimality).
#'
#' @param uncert if false (default), abilities are assumed to be known; if true,
#' handling of uncertainties of Bjermo et al. (2021) is used.
#'
#' @param ipop  matrix with item parameters for operational items
#' (used if uncert=TRUE, only).
#'
#' @param imf the vector of step-lengths; default
#' c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.45).
#'
#' @param maxiter maximal number of iterations in each inner loop, the length
#' of this vector defines the number of outer loops.
#'
#' @param eps convergence criterion (maximum violation of eq.th.), vector with
#' value for each iteration in the outer loop, but the same number for all
#' iterations is recommended.
#'
#' @param nnn number of new nodes added at each position in the adaptive grid,
#' vector with value for each iteration in the outer loop (nnn \[1] not used).
#'
#' @param nsp node spacing between new nodes, vector with value for each iteration
#' in the outer loop (nsp \[1] is the spacing between nodes of the starting grid).
#'
#' @param sss step size stopping criterion.
#' @param falpha factor alpha for adjusting the step size vector (should be > 1).
#' @param sdr stop if design repeated (flag TRUE/FALSE).
#' @param ig inner grid between -ig and ig.
#'
#' @param ex intervals of size < ex will be removed (consolidate);
#' if ex=0, no consolidation will be done.
#'
#' @param integ if true (default), integrate() is used for computation of partial
#' information matrices; if false, Riemann rule is used.
#'
#' @param show_progress if 1 (default), no output will be printed for each iteration.
#' If 2, the + symbols will be printed on a line for each Iteration
#' If 3, some output of the function will be printed.
#'
#' @return an object of class ‘optical’ is returned, which is a list with
#' following instances:
#'
#' \item{dd}{list of directional derivatives of optimal solution for each block.}
#' \item{ip}{list having matrix of item paramters in each block}
#' \item{xi}{list of optimal solution for each block.}
#' \item{t}{list of final grid of ability values which was used in each block.}
#' \item{viomax}{largest violation of eq.th. from final solution (if < eps, alg.
#'                  has converged, otherwise not).}
#' \item{h1}{list of interval boundaries for optimal solution for each block.}
#' \item{ht}{list of Refined table of interval boundaries for optimal design with
#'            calibrated items and their corresponding probabilities for each block}
#' \item{mooiter}{list for each block having monitoring iterations; information
#'                 about each iteration to produce convergence plots.}
#' \item{time}{running time of algorithm in minutes.}
#' \item{oc}{list of optimality criterion ("D", "I", "A", "L") for each block.}
#' \item{L}{list of L-matrix (not for D-optimality) for each block.}
#' @author
#' Mahmood Ul Hassan (\email{scenic555@gmail.com});
#' Frank Miller (\email{frank.miller@liu.se})
#'
#' @references
#' Ul Hassan and Miller (2021). [An exchange algorithm for optimal calibration of items in computerized achievement tests](https://www.sciencedirect.com/science/article/pii/S0167947321000116?via%3Dihub).\emph{Computational Statistics and
#' Data Analysis, 157}: 107177.
#'
#' Ul Hassan and Miller (2019). [Optimal item calibration for computerized achievement tests](https://link.springer.com/article/10.1007/s11336-019-09673-6). Psychometrika, 84, 1101-1128.
#'
#' Bjermo, Fackle-Fornius, and Miller (2021). [Optimizing Calibration Designs with Uncertainty in Abilities](https://urn.kb.se/resolve?urn=urn%3Anbn%3Ase%3Asu%3Adiva-198065). Manuscript.
#'
#' @seealso \code{\link{drawdesign}}, \code{\link{convergenceplot}},
#'          \code{\link{efficiency}}, \code{\link{drawdesign_allitems}}
#'
#' @export optical
#'
#' @examples
#' # Example No.1
#' # 2PL-model for three items with parameters (a, b) equal to (1.6, -2), (1.6, 0.5),
#' # and (1.6, 2) for the first, second, and third items, respectively.
#' # The calibration of these three items with the 2PL model is conducted in one block.
#'
#' a<-c(1.6,1.6,1.6); b<-c(-2,0.5,2)
#' ip <-cbind(a,b)
#' yyy <- optical(ip)
#' yyy
#'
#' \dontrun{
#'
#' # Example No.2
#' # 2PL-models for six items; parameters a=c(1.62, 0.66,0.92,0.90,0.98,1.40)
#' # and b=c(-0.47,-0.15,-1.71,1.60,0.62,,-1.71), respectively.
#' # The calibration of these 6 items with 2PL-models is conducted in two blocks.
#'
#' a<-c(1.62, 0.66,0.92,0.90,0.98,1.40)
#' b<-c(-0.47,-0.15,-1.71,1.60,0.62,-1.71)
#' ip<-cbind(a,b)
#' bid<-c(1,2,2,2,1,1)
#' yyy<-optical(ip,bid=bid)
#' yyy
#'
#' # Example No.3
#' # Two-parameter logistic (2PL) models were employed for nine items, with
#' # parameters set as follows: a=c(1.62, 0.66,0.92,0.82,0.90,0.98,0.36,1.40,0.64)
#' # and b=c(-0.47,-0.15,-1.71,0.33,1.60,0.62,2.84,-1.71,-1.51), respectively.
#' # The calibration of these nine items is conducted using 2PL models in three blocks.
#'
#' ip <- cbind(c(1.62,0.66,0.92,0.82,0.90,0.98,0.36,1.40,0.64),
#'             c(-0.47,-0.15,-1.71,0.33,1.60,0.62,2.84,-1.71,-1.51) )
#' bid<-c(1,2,2,3,2,1,1,3,3)
#' yyy <- optical(ip,bid=bid,show_progress=2)
#' yyy
#'
#' # Example No.4
#' # 1PL-models with a common discrimination parameter for six items. (The model
#' # assumption is that six items have the same discrimination.) Parameters are
#' # a=(1.6, NA, NA, 1.2, NA, NA) and b=(-2, -1.5, -1, 0.5, 1, 1.5), respectively.
#' # 'NA' for discrimination means that the item has the same parameter as the
#' # preceding item. The calibration of these 6 items with 2PL-models is conducted
#' # in 3 blocks.
#'
#' ip<-cbind(c(1.6, NA, NA, 1.2, NA, NA),c(-2, -1.5, -1, 0.5, 1, 1.5))
#' bid<-c(1,1,1,2,2,2)
#' yyy <- optical(ip,bid=bid,show_progress=2)
#' yyy
#'
#' # Example No.5
#' # 3PL-models for three items; the parameters for these items are a=(1, 2, 2.5),
#' # b=(-1.5, 0.5, 2), and c=(0.2, 0.1, 0.05), respectively. The Calibration of
#' # items following 3PL-models is performed in one block.
#'
#' a<-c(1, 2, 2.5); b<-c(-1.5, 0.5, 2); c<-c(0.2, 0.1, 0.05)
#' ip<-cbind(a,b,c)
#' yyy <- optical(ip,show_progress=2)
#' yyy
#'
#' # Example No.6
#' # 1PL-models are applied for four items, 2PL-models to three items, 3PL-models
#' # to three items,and 2-3PL-models to four items. The calibration of items
#' # is performed in four block.
#'
#' a<-c(c(1, 1.8, 1.4, 1, 2, NA, 1.7, NA, 2.5, NA, 0.8, 1.5, 2, 2.8))
#' b<-c(-1.5, -0.9, -0.1, -1.5, 0.5, -0.25, 1.1, 0.25, 2, 1.5, -0.5,-2, 0.5, 2)
#' c<-c(NA, NA, NA, 0.2, 0.1, NA, NA, NA, 0.05, NA, NA, NA, 0.1, 0.2)
#' ip<-cbind(a,b,c)
#' bid<-c(1, 2, 2, 3, 3, 1, 2, 1, 3, 1, 4, 4, 4, 4)
#' yyy <- optical(ip,show_progress=2)
#' yyy
#'
#' }



optical<-function(ip,bid=NULL,oc="D", uncert= FALSE,ipop,
                        imf = c(0.005,0.01, 0.02, 0.05, 0.1, 0.2, 0.45),
                        maxiter=rep(300, 6), eps=rep(0.002, 6),
                        nnn=c(0, 50, 50, 200, 200, 200),
                        nsp=c(0.001, 1e-04, 1e-04, 1e-05, 1e-05, 1e-05),
                        sss=0.001,falpha=1.08, sdr=TRUE, ig=3, ex=0,
                        integ=TRUE,show_progress=1){


starttime <- proc.time()
# if block is missing then all items are in one block
if(is.null(bid)) {bid<-rep(1,nrow(ip))}

data<-cbind(ip,bid)

ncol<-ncol(ip)
Nblock<-length(unique(bid))
rownames(ip)<-1:nrow(ip)

bclib<-list();ipar<-list();
xi<-list();dd<-list()
h1<-list();ht<-list();t<-list()
opcrit<-list();mooiter<-list();L<-list()

Bnames<-paste("Block", 1:Nblock,sep="")

time<-matrix(0,(Nblock+1),3)
colnames(time)<-c("user","system","elapsed")
rownames(time)<-c(Bnames,"total")



# For saving max violation in calibration for each block
viomax<-matrix(0,Nblock,1)
rownames(viomax)<-Bnames
colnames(viomax)<-c("viomax")


for(i in 1:Nblock){
par_bitem<-ip[bid==i,]


# If all row of 3rd column is NA then remove that column
if(sum(is.na(par_bitem[,ncol])) == nrow(par_bitem)){par_bitem<-par_bitem[,-ncol]}

if(show_progress==2 | show_progress==3){
cat(Bnames[i],"\n")

}

yy <- optical0(par_bitem,oc, uncert,ipop, imf , maxiter, eps,
              nnn, nsp, sss,falpha, sdr, ig, ex,
              integ,show_progress)


viomax[i,]<-yy$viomax
ipar[[i]]<-par_bitem

user<- as.numeric(yy$time[1])
system <- as.numeric(yy$time[2])
elapsed <- as.numeric(yy$time[3])
time[i,]<-c(user,system,elapsed)


dd[[i]]<-yy$dd
xi[[i]]<-yy$xi
h1[[i]]<-yy$h1
ht[[i]]<-yy$ht
mooiter[[i]]<-yy$mooiter
opcrit[[i]]<-yy$oc
L[[i]]<-yy$L
t[[i]]<-yy$t


}

runtime <- (proc.time() - starttime)/60


user<- as.numeric(runtime[1])
system <- as.numeric(runtime[2])
elapsed <- as.numeric(runtime[3])


time[(Nblock+1),]<-c(user,system,elapsed)



output <-list()
names(ht)<-paste("block", 1:Nblock,sep="")

output$time<-time
output$viomax<-viomax



output$xi<-xi;output$dd<-dd;output$ip<-ipar;
output$ht<-ht;output$h1<-h1;output$t<-t;
output$L<-L;output$oc<-opcrit;output$mooiter<-mooiter

class(output) <-"optical"
return(output)
}




