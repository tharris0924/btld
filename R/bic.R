#' Bayesian Information Criterion (BIC)
#'
#' Calculate the BIC for a model.
#'
#' @param loglik log-likelihood
#' @param k number of paramters
#' @param n sample size
#' @return BIC
#' @import mixtools
#' @export
#' @examples
#' gmm<-mixtools::normalmixEM(runif(1000))
#' aic(gmm$loglik,3)



bic_bt<-function(loglik,k,n){
  return((-2*loglik)+(k*log(n)))
}