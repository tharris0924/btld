#' btld_scales
#'
#' Function to determine the scale parameters from the H-plot
#'
#' @param est_theta estimated breakpoint (mode) values from the H-plot
#' @param F_thet functional values at breakpoints

#' @return estimated scale parameters
#'
#' @export
#' @examples
#' th<-c(0.27,0.7)
#' pth <- c(0.605,0.85)
#' btldScales(th,pth)
btldScales <- function (est_theta, F_thet){
    est_alpha_1 <- (2*F_thet[1])/est_theta[1]
    est_alpha_2 <- (2*(1-F_thet[2]))/(1-est_theta[2])
    est <-as.matrix(c(est_alpha_1,est_alpha_2))
  return(est)
}
