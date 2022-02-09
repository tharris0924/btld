#' Determine the parameters of the btld
#'
#' Using the various functions built in this package, this function calculates the mode and scale parameters for the btld.
#'
#' @param xs input data
#' @param normalize do we normalize?
#' @return dataframe containing the scales and modes
#' @import mixtools
#' @export
#' @examples
#' library(mixtools)
#' data(NOdata)
#' btld.params(NOdata$NO, normalize=TRUE)
btld.params<-function(xs, normalize=F){
  if(normalize==T){
    xs<-unit.normalize(xs)
  }
  f_thetas<-find.f_thetas(xs)
  modes<-find.thetas(xs)
  modes<-c(min(modes),max(modes))
  alphas<-as.matrix(btld_scales(modes,f_thetas))
  parm.df<-data.frame(modes=modes,alphas=alphas)
  return(parm.df)
}