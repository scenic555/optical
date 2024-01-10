#' print an optical object
#'
#'
#' This function print the table of interval boundaries for
#' optimal design with items and probabilities for each block.
#'
#'
#' @param x optical object.
#' @param digits significant digits in printout.
#' @param ... other print arguments.
#' @return No return value, called for side effects.
#' @usage \method{print}{optical}(x,digits=max(3, getOption("digits")-3),\dots)
#' @seealso \code{\link{optical}}
#' @export

# print.optical<-function (x,digits=max(3, getOption("digits") - 3),...){
#   y<-x$bclib
#   print(y, digits)
# }




print.optical<-function (x,digits=max(3, getOption("digits") - 3),...){

  tab<-x$ht

  for (i in 1: length(tab)){
    row <- paste(rep("=", 67), collapse = "")

    cat(paste0("Block",i,sep=""), "\n")
    cat(row, "\n")
    cat("Table of interval boundaries for", paste0(x$oc[[i]], "-optimal design with items and\n",
                                                   "probabilities (expected proportion of examinees in this interval)"), "\n")
    cat(row, "\n")

    print(tab[[i]], digits)
    cat("\n")  # Insert a blank line after each output
  }
}


