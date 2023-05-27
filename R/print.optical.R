#' print a optical object
#'
#' This function print the table of interval boundaries for
#' optimal design with items and probabilities.
#' @param x optical object.
#' @param digits significant digits in printout.
#' @param ... other print arguments.
#' @return No return value, called for side effects.
#' @usage \method{print}{optical}(x,digits=max(3, getOption("digits")-3),\dots)
#' @seealso \code{\link{optical}}
#' @export
print.optical<-function (x,digits=max(3, getOption("digits") - 3),...){
  row <- paste(rep("=", 67), collapse = "")
  cat(row, "\n")
cat(
" Table of interval boundaries for", paste0(x$oc,"-optimal design with items and
 probabilities (expected proportion of examinees in this interval)"),"\n")
cat(row, "\n")

print(x$ht, digits)
}
