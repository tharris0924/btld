#' Change scale of data to be over the range 0 to 1
#'
#' Recsales data using the \code{caret} package
#'
#' @param x input data
#' @return data rescaled to lie between 0 and 1
#'
#' @importFrom caret preProcess
#' @importFrom stats predict
#' @export
#' @examples
#' x<-runif(1000,5,10)
#' unit.normalize(x)
unit.normalize<-function(x){
  xs.df<-data.frame(x)
  pre<-preProcess(xs.df,method="range")
  unit.df<- predict(pre, xs.df)
  unit.mat<-as.matrix(unit.df)
  return(unit.mat)
}