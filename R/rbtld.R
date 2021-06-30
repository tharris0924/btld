#' rbltd
#'
#' Random number generator for the bltd
#'
#' @param size size of sample to generate.
#' @param theta1 Mode paramter 1.
#' @param theta2 mode parameter 2.
#' @param alpha1 scale parameter 1
#' @param alpha2 scale parameter 2
#' @return The random variates from a bltd
#'
#' @export
#' @examples
#' rbltd(size=1000, alpha1 = 5, alpha2 = 1, theta1=0.3, theta2 = 0.7)
rbltd <- function (size, theta1, theta2, alpha1, alpha2){

  u<-runif(n=size)
  alpha0<-(1-alpha1*theta1/2 -alpha2*(1-theta2)/2)/(theta2-theta1)
  u_df <- data.frame(unif=u)

  # specify valid bounds
  lower_df <- u_df[u_df$unif<=alpha1*theta1/2,]
  middle_df<-u_df[(alpha1*theta1/2)<u_df$unif & u_df$unif<=(alpha1*theta1/2+alpha0*(theta2-theta1)),]
  upper_df <- u_df[u_df$unif>(alpha1*theta1/2+alpha0*(theta2-theta1)),]

  # generate values
  lower_df<-sqrt((2*lower_df*theta1)/alpha1)
  middle_df<-(middle_df-(alpha1*theta1)/2)/alpha0 + theta1
  upper_df<-1-sqrt((2*(1-upper_df)*(1-theta2))/alpha2)

  df <- c(lower_df,middle_df,upper_df)
  rvs <- data.frame(GenRvs=df)
  return(rvs)
}