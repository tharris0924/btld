#' ptridist
#'
#' random number generator for the triangular distribution
#'
#' @param values input vector
#' @param theta mode parameter (between 0 and 1)
#'
#' @return df of random variates
#' @export
#' @examples
#'
#' ptridist(runif(1000),0.3)
#' ptridist(runif(1000), 0.7)

ptridist <- function (values, theta){
  u_df <- data.frame(unif=values)
  lower_df <- u_df[u_df$unif<theta,]
  upper_df <- u_df[u_df$unif>theta,]
  lower_df<-(lower_df^2)/theta
  upper_df<-1-((1-upper_df)^2/(1-theta))
  lst <- c(lower_df,upper_df)
  return(lst)
}