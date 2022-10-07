#' Akaike Information Criterion
#'
#' Calculate the AIC for a model.
#'
#' @param loglik log-likelihood
#' @param k number of paramters
#' @return AIC
#' @import mixtools
#' @export
#' @examples
#' gmm<-mixtools::normalmixEM(runif(1000))
#' aic(gmm$loglik,3)

aic<-function(loglik,k){
  return((-2*loglik)+(2*k))
}