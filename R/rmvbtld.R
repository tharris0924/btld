#' Random generation from MVBTLD
#'
#' This function is the random generator for length n d-dimensional MVBTLD random variables.
#'
#' @param n size of dataset desired
#' @param alpha scale matrix. Must be d-by-2 dimensions.
#' @param theta mode matrix. 2 Must be d-by-2 dimensions.
#' @param sigma value on the off diagonal for Gaussian covariance matrix used in copula generation. Can be vector.
#' @param dim number of dimensions
#' @param ... kws for \code{normalCopula} and \code{rCopula}
#' @return matrix n by d random variates from the MVBTLD
#'
#' @importFrom copula normalCopula rCopula
#' @export
#' @examples
#' alpha<-matrix(c(3,3,3,3),nrow=2,ncol=2, byrow=TRUE)
#' theta<-matrix(c(0.3,0.7,0.3,0.7),ncol=2,nrow=2,byrow=TRUE)
#' rmvbtld(1000, alpha=alpha, theta=theta, sigma=0.5,dim=2)

rmvbtld<-function (n,theta,alpha,sigma,dim,...){
  norm.cop <- normalCopula(sigma, dim,...) # intialize copula function
  u <- rCopula(n, norm.cop)
  # generate 100 rvs from the cop
  # getSigma(norm.cop)
  if (nrow(alpha)<ncol(alpha)){
    print("Warning: scale matrix must be columnwise")
    alpha<-t(alpha)
  }
  if (nrow(theta)<ncol(theta)){
    print("Warning: mode matrix must be columnwise")
    theta<-t(theta)
  }
  mat<-NULL
  defaultW <- getOption("warn")
  options(warn = -1)
  for(i in seq_len(dim)){
    mat<-cbind(mat,qbtld(x=u[,i], alpha = alpha[i,], theta=theta[i,]))
  }
  options(warn = defaultW)
  return(mat)
}