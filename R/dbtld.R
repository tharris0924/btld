#' dbtld
#'
#' Probability density function of the BTLD
#'
#' @param xvalues input values between 0 and 1
#' @param alpha scale vector parameter
#' @param theta Mode vector paramter
#'
#' @return df of pdf function values
#'
#' @importFrom stats runif
#' @export
#' @examples
#'
#' dbtld(runif(1000),  alpha = c(5,1), theta =c(0.3, 0.7))

dbtld <- function (xvalues, alpha,theta){
    alpha0<-(1-(alpha[1]*theta[1]/2) -(alpha[2]*(1-theta[2]))/2)/(theta[2]-theta[1])
    r <- which(xvalues<theta[1])
    x1<-alpha[1]*xvalues[r]/(theta[1])
    r<-which(xvalues>theta[2])
    x3<-(alpha[2]*(1-xvalues[r]))/(1-theta[2])
    n <- length(xvalues)-length(x1)-length(x3)
    x2<-rep(alpha0, times = n)
    joined_pdf <- c(x1,x2,x3)
    return(joined_pdf)
}