#' dbtld
#'
#' Probability density function of the BTLD
#'
#' @param xvalues input values between 0 and 1
#' @param alpha scale vector parameter
#' @param theta Mode vector paramter
#'
#' @return df of pdf function values
#'
#' @importFrom stats runif
#' @export
#' @examples
#'
#' dbtld(runif(1000),  alpha = c(5,1), theta =c(0.3, 0.7))
dbtld <- function (xvalues,  alpha,theta){
  alpha1<-alpha[1]
  alpha2<-alpha[2]
  theta1<-theta[1]
  theta2<-theta[2]
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