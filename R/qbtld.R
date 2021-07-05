#' @title quantile function for the btld
#'
#' @description
#' Quantile function (icdf) for the btld
#'
#' @param x A vector of inputs.
#' @param theta Mode vector
#' @param alpha Scale vector
#' @return The icdf of \code{x}.
#'
#' @export
#' @examples
#' alpha<-c(5,1)
#' theta<-c(0.3,0.7)
#' qbtld(x=runif(1000), alpha, theta)
qbtld <- function (x, theta, alpha){
  theta1<-theta[1]
  theta2<-theta[2]
  alpha1<-alpha[1]
  alpha2<-alpha[2]
  alpha0<-(1-alpha1*theta1/2 -alpha2*(1-theta2)/2)/(theta2-theta1)
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
  return(df)
}