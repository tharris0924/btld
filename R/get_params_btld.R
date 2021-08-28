#' get_params_btld
#'
#' Function to determine the scale parameters from the H-plot for the multivariate case
#'
#' @param data data that exhibits modes stored as a matrix
#'
#' @return parameters for a btld model
#'
#' @export
#' @examples
#' daa<- rbtld(size=1000, alpha = c(5,1), theta =c(0.3, 0.7))
#' get_params_btld(matrix(daa))
#'
get_params_btld<-function (data){
  modes<-sort(unique(knn_modeseeking(data)[,3]))
  if(length(modes)>2){
    modes<-modes[2:3]
  }
  ecdf<-empcdf(data)
  probs<-unique(ecdf[ecdf[,1]== modes,])
  scales<-btld_scales(modes,sort(probs[,2]))
  params<-c(modes,scales)
  return(params)
}