
#' Random generation from the compositional MVBTLD.
#'
#' This function is a random generator for length n (d)-dimensional MVBTLD random variables. It is directly called in
#' the function \code{mu_mvbtld.R}.
#'
#' @param n size of dataset desired
#' @param alpha scale matrix. Must be d-by-2 dimensions.
#' @param theta mode matrix. 2 Must be d-by-2 dimensions.
#' @param sigma value on the off diagonal for Gaussian covariance matrix used in copula generation.
#' @param dim number of dimensions of copula. Must be same dimensions as the mode and scale parameters.
#' @param ... kws for \code{normalCopula} and \code{rCopula}.
#' @return matrix n by d+1 random variates from the compositional MVBTLD.
#'
#' @export
#' @examples
#' alpha<-matrix(c(3,3,3,3),nrow=2,ncol=2, byrow=TRUE)
#' theta<-matrix(c(0.3,0.7,0.3,0.7),ncol=2,nrow=2,byrow=TRUE)
#' rcmvbtld(1000, alpha=alpha, theta=theta, sigma=0.5,dim=2)

rcmvbtld<-function (n,theta,alpha,sigma,dim,...){
  y<-rmvbtld(10000,theta,alpha,sigma,dim,...) # generate sufficiently many datapoints since not all observations across
  # dimensions are compositional
  comp <- 1-rowSums(y)
  compdf <- data.frame(y, comp)
  compdf <- compdf[!compdf$comp<0,]
  resampled <- compdf[sample(seq_len(nrow(compdf)), n),] #reintroduce some randomness
  rownames(resampled)<-NULL
  return(resampled)
}