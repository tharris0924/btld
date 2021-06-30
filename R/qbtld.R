#' Quantile function (icdf) for the bltd
#'
#' @param x A vector of inputs.
#' @param theta1 Mode paramter 1.
#' @param theta2 mode parameter 2.
#' @param alpha1 scale parameter 1
#' @param alpha2 scale parameter 2
#' @return The icdf of \code{x}.
#'
#' @export
#' @examples
#' qbltd(x=runif(1000), alpha1 = 5, alpha2 = 1, theta1=0.3, theta2 = 0.7)
qbltd <- function (x, theta1, theta2, alpha1, alpha2){
  alpha0<-(1-alpha1*theta1/2 -alpha2*(1-theta2)/2)/(theta2-theta1)
  print(alpha0)
  u_df <- data.frame(x=x)

  # specify valid bounds
  lower_df <- u_df[u_df$x<=alpha1*theta1/2,]
  middle_df<-u_df[(alpha1*theta1/2)<u_df$x & u_df$x<=(alpha1*theta1/2+alpha0*(theta2-theta1)),]
  upper_df <- u_df[u_df$x>(alpha1*theta1/2+alpha0*(theta2-theta1)),]

  # generate values
  lower_df<-sqrt((2*lower_df*theta1)/alpha1)
  middle_df<-(middle_df-(alpha1*theta1)/2)/alpha0 + theta1
  upper_df<-1-sqrt((2*(1-upper_df)*(1-theta2))/alpha2)

  df <- c(lower_df,middle_df,upper_df)
  qs <- data.frame(GenRvs=df)
  return(qs)

}

