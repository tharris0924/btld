#' Determine ecdf values for the mode parameters
#'
#' Search through the ecdf to find values that are around the modes and calculate the average density around the modes.
#'
#' @param x input data
#' @return cdf values for the mode parameters
#'
#' @export
#' @examples
#' x<-runif(1000,0,1)
#' find.f_thetas(x)
find.f_thetas<-function(x){
  # unit<-unit.normalize(x)
  modes<-find.thetas(x)
  ecdf<-empcdf(x)
  rounded<-round(ecdf,digits=2)
  f_thet<-matrix(nrow=2,ncol=1)
  ecdf<-as.data.frame(ecdf)
  for(i in seq_len(2)){
    idxs<-which(rounded[,1]==as.character(modes[i]))
    ids<-ecdf[idxs,2]
    f_thet[i,]<-mean(ids)
  }
  return(f_thet)
}
