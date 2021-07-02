#' dtridist
#'
#' PDF for the triangular distribution
#'
#' @param values input vector
#' @param theta mode parameter (between 0 and 1)
#'
#' @return df of random variates
#'
#' @importFrom stats runif
#' @export
#' @examples
#' dtridist(runif(1000),0.3)
#' dtridist(runif(100), 0.7)

dtridist <- function (values, theta){
  u_df <- data.frame(unif=values)
  lower_df <- u_df[u_df$unif<theta,]
  upper_df <- u_df[u_df$unif>theta,]
  lower_df<-(lower_df*2)/theta
  upper_df<-(2*(1-upper_df)/(1-theta))
  lst <- c(lower_df,upper_df)
  return(lst)
}