#' pbtld
#'
#' Cumulative density function for the BTLD
#'
#' @param xvalues input values
#' @param alpha scale vector parameter 1
#' @param theta Mode vector paramter 1.

#' @return Vector of cumulative probabilities from the BTLD
#'
#' @export
#' @examples
#' pbtld(runif(1000), alpha = c(5,1), theta =c(0.3, 0.7))
pbtld <- function (xvalues, alpha,theta){
  alpha0<-(1-(alpha[1]*theta[1]/2) -(alpha[2]*(1-theta[2]))/2)/(theta[2]-theta[1])
  r <- which(xvalues<theta[1])
  f1<-alpha[1]*(xvalues[r])^2/(2*theta[1])
  r<-which(xvalues>theta[2])
  f3<-1-(alpha[2]*(1-xvalues[r])^2)/(2*(1-theta[2]))
  ls<-theta[1]<xvalues& xvalues<theta[2]
  r<-which(ls)
  f2<-alpha0*(xvalues[r]-theta[1])+(alpha[1]*theta[1])/2
  cdf <- c(f1,f2,f3)
  return(cdf)
}