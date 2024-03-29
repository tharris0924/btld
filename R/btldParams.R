#' Determine the parameters of the btld
#'
#' Using the various functions built in this package, and a KDE to initialize the modal parameters, this function calculates the mode and scale parameters for the btld.
#'
#' @param xs input data
#' @param normalize do we normalize?
#' @return dataframe containing the scales and modes
#' @import mixtools graphics
#' @export
#' @examples
#' library(mixtools)
#' data(NOdata)
#' btldParams(NOdata$NO,normalize=TRUE)


btldParams<-function(xs, normalize=F){
  if(normalize==T){
    xs<-unit.normalize(xs)
  }
  hist(xs)
  str1<-readline('Please enter the first mode: ')
  str2<-readline('Please enter the second mode: ')
  modes<-matrix(c(as.numeric(str1), as.numeric(str2)))
  f_thetas<-findFThetas(xs)
  alphas<-as.matrix(btldScales(modes,f_thetas))
  parm.df<-data.frame(modes=modes,alphas=alphas)
  return(parm.df)
}
