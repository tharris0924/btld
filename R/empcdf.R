#' empcdf
#'
#' Function to determine the empirical cdf of data
#'
#' @param dataframe list of datapoints


#' @return data.frame object with two columns: sorted values from input and ecdf
#'@importFrom stats ecdf
#' @export
#' @examples
#' s<-seq(1,10,1)
#' empcdf(matrix(s))
#'
empcdf<-function(dataframe){
  Fn<-ecdf(dataframe)
  probs<-Fn(dataframe)
  mat<-matrix(c(dataframe,probs),nrow=length(dataframe), ncol=2)
  colnames(mat)<-c('X', 'FnX')
  return(mat)
}