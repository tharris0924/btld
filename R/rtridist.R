#' rtridist
#'
#' random number generator for the triangular distribution
#'
#' @param size size of sample
#' @param theta mode parameter (between 0 and 1)
#'
#' @return df of random variates
#'
#' @export
#' @examples
#' rtridist(1000,0.3)
#' rtridist(100, 0.7)
rtridist <- function (size, theta){
  u<-runif(n=size)
  u_df <- data.frame(unif=u)
  lower_df <- u_df[u_df$unif<theta,]
  upper_df <- u_df[u_df$unif>theta,]
  lower_df<-sqrt(lower_df*theta)
  upper_df<-1-sqrt((1-theta)*(1-upper_df))
  rvs <- matrix(c(lower_df,upper_df))
  return(rvs)
}