#' tridist_cdf
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
#'
#' tridist_cdf(runif(1000),0.3)
#' tridist_cdf(runif(1000), 0.7)

tridist_cdf <- function (values, theta){
  u_df <- data.frame(unif=values)
  lower_df <- u_df[u_df$unif<theta,]
  upper_df <- u_df[u_df$unif>theta,]
  lower_df<-(lower_df^2)/theta
  upper_df<-1-((1-upper_df)^2/(1-theta))
  lst <- c(lower_df,upper_df)
  return(lst)
}