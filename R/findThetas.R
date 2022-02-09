#' Determine modes of btld
#'
#' Using knn_modeseeking, calculate the local maxima.
#'
#' @param xs input data.
#' @return modes
#'
#' @importFrom graphics hist
#' @export
#' @examples
#' x<-runif(1000,0,1)
#' find.thetas(x)
find.thetas<-function(xs){
  freq<- hist(xs, include.lowest=TRUE, plot=FALSE, breaks = 10)
  density<-freq$density
  modeidxs<-which(diff(sign(diff(density)))==-2)+1
  mids<-freq$mids
  theta<-mids[modeidxs]
  return(theta)
}