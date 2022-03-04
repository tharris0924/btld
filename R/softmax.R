#' softmax function
#'
#' This function computes the softmax normalized values for use in the multinomial
#'
#'@param x input data
#'@param exp logical to determine if to use exponential function
#'@param base base of softmax when \code{exp=F}
#'
#'
#'@export
#'@examples
#' alpha<-matrix(c(3,3,3,3),nrow=2,ncol=2, byrow=TRUE)
#' theta<-matrix(c(0.3,0.7,0.3,0.7),ncol=2,nrow=2,byrow=TRUE)
#' df<-rmvbtld(1000, alpha=alpha, theta=theta, sigma=0.5,dim=2)
#' softmax(df)

softmax<-function (x,exp=T,base=NULL){
  if(exp==T){
    sigma<-exp(x)/rowSums(exp(x))
  }
  else{
    sigma<-base^(x)/rowSums(base^(x))
  }
  return(sigma)
}