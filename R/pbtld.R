#' pbtld
#'
#' Cumulative density function for the BTLD
#'
#' @param vec input values
#' @param alpha1 scale parameter 1
#' @param alpha2 scale parameter 2
#' @param theta1 Mode paramter 1.
#' @param theta2 mode parameter 2.

#' @return Vector of cumulative probabilities from the BTLD
#'
#' @importFrom stats runif
#' @export
#' @examples
#' pbtld(runif(1000), alpha1 = 5, alpha2 = 1, theta1=0.3, theta2 = 0.7)
pbtld <- function (vec, alpha1, alpha2, theta1, theta2){
  u<-vec
  alpha0<-(1-alpha1*theta1/2 -alpha2*(1-theta2)/2)/(theta2-theta1)
  u_df <- data.frame(unif=u)

  # specify valid bounds
  lower_df <- u_df[u_df$unif<=theta1,]
  middle_df<-u_df[theta1<u_df$unif & u_df$unif<=theta2,]
  upper_df <- u_df[u_df$unif>theta2,]
  # generate values
  lower_cdf<-alpha1*lower_df^2/(2*theta1)
  middle_cdf<-alpha0*(middle_df-theta1)+alpha1*theta1/2
  upper_cdf<-1-(alpha2*(1-upper_df)^2)/(2*(1-theta2))

  cdf <- c(lower_cdf,middle_cdf,upper_cdf)
  return(cdf)
}