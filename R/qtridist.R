#' qtridist
#'
#' icdf for the triangular distribution
#'
#' @param values input vector
#' @param theta mode parameter (between 0 and 1)
#'
#' @return df of random variates
#'
#' @importFrom stats runif
#' @export
#' @examples
#' qtridist(runif(1000),0.3)
#' qtridist(runif(100), 0.7)
qtridist <- function (values, theta){
  u_df <- data.frame(unif=values)
  lower_df <- u_df[u_df$unif<theta,]
  upper_df <- u_df[u_df$unif>theta,]
  lower_df<-sqrt(lower_df*theta)
  upper_df<-1-sqrt((1-theta)*(1-upper_df))
  df <- c(lower_df,upper_df)
  return(df)
}