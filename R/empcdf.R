#' empcdf
#'
#' Function to determine the empirical cdf of data
#'
#' @param df list of datapoints


#' @return data.frame object with two columns: sorted values from input and ecdf
#'@importFrom stats ecdf
#' @export
#' @examples
#' s<-seq(1,10,1)
#' empcdf(matrix(s))
#'
empcdf<-function(df){
  Fn<-ecdf(df)
  probs<-Fn(df)
  mat<-matrix(c(df,probs),nrow=length(df), ncol=2)
  colnames(mat)<-c('X', 'FnX')
  return(mat)
}