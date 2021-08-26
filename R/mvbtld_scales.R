#' mvbtld_scales
#'
#' Function to determine the scale parameters from the H-plot for the multivariate case
#'
#' @param est_theta estimated breakpoint (mode) values stored as a vector for a d-dimensional model from the H-plot
#' @param F_thet functional values at breakpoints for each theta
#'
#' @return scale parameters for a mvbtld model
#'
#' @export
#' @examples
#' th<-matrix(c(0.3,0.7,0.3,0.7),ncol=2,nrow=2,byrow=TRUE)
#' pth <- matrix(c(0.17, 0.23, 0.43, 0.53),ncol=2,nrow=2,byrow=TRUE)
#' mvbtld_scales(est_theta=th,F_thet=pth)

mvbtld_scales <- function (est_theta, F_thet){
  p<-nrow(est_theta)
  est<-matrix(nrow=p,ncol=ncol(est_theta))
  for(i in seq_len(p)){
    est[i,]<-btld_scales(est_theta[i,], F_thet[i,])
  }
  return(est)
}