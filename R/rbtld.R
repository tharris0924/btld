#' Random number generator for the bimodal triangular linked distribution
#'
#' Using the icdf we generate random variates for the btld
#'
#' @param size size of sample to generate.
#' @param alpha scale vector parameter
#' @param theta Mode vector paramter
#' @return The random variates from a bltd

#' @importFrom stats runif
#' @export
#' @examples
#' rbtld(size=1000, alpha = c(5,1), theta =c(0.3, 0.7))

rbtld <- function (size, alpha, theta){
    u<-runif(size)
    alpha0<-(1-(alpha[1]*theta[1]/2) -(alpha[2]*(1-theta[2]))/2)/(theta[2]-theta[1])
    A<-(alpha[1]*theta[1]/2)
    r<-which(u<A)
    x1<-sqrt((2*theta[1]*u[r])/alpha[1])
    C<-A+alpha0*(theta[2]-theta[1])
    r<-which(u>C)
    x3<-1-sqrt((2*(1-u[r])*(1-theta[2]))/alpha[2])
    ls<-A<u & u<C
    r<-which(ls)
    x2<-theta[1]+(u[r]-A)/alpha0
    x<-c(x1,x2,x3)
  return(x)
}
