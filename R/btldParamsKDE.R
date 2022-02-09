#' Determine the parameters of the btld
#'
#' Using the various functions built in this package, and a KDE to initialize the modal parameters, this function calculates the mode and scale parameters for the btld.
#'
#' @param xs input data
#' @param normalize do we normalize?
#' @return dataframe containing the scales and modes
#' @import mixtools
#' @export
#' @examples
#' library(mixtools)
#' data(NOdata)
#' btldParamsKDE(NOdata$NO, normalize=TRUE)
btldParamsKDE<-function(xs, normalize=F){
  if(normalize==T){
    xs<-unit.normalize(xs)
  }
  f_thetas<-findFThetas(xs)
  modes<-findThetas(xs)
  modes<-c(min(modes),max(modes))
  alphas<-as.matrix(btldScales(modes,f_thetas))
  parm.df<-data.frame(modes=modes,alphas=alphas)
  return(parm.df)
}