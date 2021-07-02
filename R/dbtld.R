#' dbtld
#'
#' Probability density function of the BTLD
#'
#' @param xvalues input values between 0 and 1
#' @param alpha1 scale parameter
#' @param alpha2 scale parameter
#' @param theta1 scale parameter
#' @param theta2 scale parameter
#'
#' @return df of pdf function values
#'
#' @importFrom stats runif
#' @export
#' @examples
#'
#' dbtld(runif(1000),  theta1 = 0.3, theta2 = 0.8, alpha1 = 3, alpha2 =3)
dbtld <- function (xvalues, alpha1, alpha2, theta1, theta2){
  alpha0<-(1-alpha1*theta1/2 -alpha2*(1-theta2)/2)/(theta2-theta1)
  df <- data.frame(unif=xvalues)
  # specify valid bounds
  lower_df <- df[df$unif<theta1,]
  middle_df<-df[theta1 <=df$unif & df$unif<=theta2,]
  upper_df <- df[df$unif>theta2,]
  inputs<-c(lower_df,middle_df,upper_df)
  # generate values
  n <- length(xvalues)-length(lower_df)-length(upper_df)
  lower_pdf<-alpha1*lower_df/(theta1)
  middle_pdf<-rep(alpha0, times = n)
  upper_pdf<-(alpha2*(1-upper_df))/(1-theta2)

  joined_pdf <- c(lower_pdf,middle_pdf,upper_pdf)
  return(joined_pdf)
}