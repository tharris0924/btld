
#' Random generation from the compound Multinomial-MVBTLD.
#'
#' This function is a random generator for length n (d)-dimensional MVBTLD random variables.
#'
#' @param n size of dataset desired
#' @param size integer indicating the total number of objects to draw from in the multinomial
#' @param alpha scale matrix. Must be d-by-2 dimensions.
#' @param theta mode matrix. Must be d-by-2 dimensions.
#' @param sigma value on the off diagonal for Gaussian covariance matrix used in copula generation.
#' @param dim number of dimensions of copula. Must be same dimensions as the mode and scale parameters.
#' @return matrix n by d+1 random variates from the compound MN-MVBTLD.
#'
#' @importFrom stats rmultinom
#' @export
#' @examples
#' alpha<-matrix(c(3,3,3,3),nrow=2,ncol=2, byrow=TRUE)
#' theta<-matrix(c(0.3,0.7,0.3,0.7),ncol=2,nrow=2,byrow=TRUE)
#' mn_mvbtld(100, size=10, alpha=alpha, theta=theta, sigma=0.5,dim=2)

mn_mvbtld<-function(n,size,alpha,theta,sigma,dim){
  df <- matrix(nrow=n,ncol=dim+1)
  for(i in seq_len(n)){
    p <- crmvbtld(1, alpha = alpha,theta = theta,sigma=sigma,dim = dim)
    df[i,] <- t(rmultinom(n=1,size = size,prob=p))
  }
    return(df)
}
