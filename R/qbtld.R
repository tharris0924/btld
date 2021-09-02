#' @title quantile function for the btld
#'
#' @description
#' Quantile function (icdf) for the btld
#'
#' @param x A vector of inputs.
#' @param theta Mode vector
#' @param alpha Scale vector
#' @return The icdf of \code{x}.
#'
#' @export
#' @examples
#' alpha<-c(5,1)
#' theta<-c(0.3,0.7)
#' qbtld(x=runif(1000), alpha, theta)
qbtld <- function (x, theta, alpha){
  alpha0<-(1-(alpha[1]*theta[1]/2) -(alpha[2]*(1-theta[2]))/2)/(theta[2]-theta[1])
  A<-(alpha[1]*theta[1]/2)
  r<-which(x<A)
  q1<-sqrt((2*theta[1]*x[r])/alpha[1])
  C<-A+alpha0*(theta[2]-theta[1])
  r<-which(x>C)
  q3<-1-sqrt((2*(1-x[r])*(1-theta[2]))/alpha[2])
  ls<-A<x & x<C
  r<-which(ls)
  q2<-theta[1]+(x[r]-A)/alpha0
  qs<-c(q1,q2,q3)
  return(qs)
}