#' kNN Mode-Seeking Algorithm
#'
#' This function calculates the knn modeseeking estimates which return the modes we use in the BTLD. The main reference for this function is:
#'
#' Nordhaug Myhre, Jonas / Øyvind Mikalsen, Karl / Løkse, Sigurd / Jenssen, Robert
#' Robust clustering using a kNN mode seeking ensemble
#' 2018

#' Pattern Recognition , Vol. 76
#' p. 491-505

#'
#' @param data input data
#' @param p proportion of data to use as span
#' @return matrix with 3 columns for the indices, data and modes.
#'
#' @importFrom fields rdist
#' @export
#' @examples
#'df<-rbtld(size=1000, alpha = c(5,1), theta =c(0.3, 0.7))
#' knn_modeseeking(df)
#'
knn_modeseeking<-function (data,p=0.15){
  n<-length(data)
  k<-round(p*n)

  distance <- rdist(data) # symmetric distance matrix
  knn_density <- vector() # empty matrix for density estimates
  for (i in seq_len(n)){ #find sorted distances posted distance calculation
    dist <- distance[,i] # take each column in distance matrix (where distance matrix is symmetric)
    sorted <- sort(dist) # sort the columns by distance to determine the nearest n points
    kth_neighbour<-sorted[k] # find kth nearest neighbour
    knn_density[i]<-1/kth_neighbour^2 # find the KDE
  }

  # neighbourhood in nonparametric sense
  x_initial <- cbind(data, knn_density)
  n<-nrow(x_initial)
  ptr_mat <- matrix(nrow = n, ncol = 3)
  colnames(ptr_mat)<-c("idx", "x", "mode")
  for (iter in 1:n){
    ptr<-iter # initialize pointer
    kill_loop<-0 # initialize loop break conditions
    while (kill_loop==0){
      x_i <- cbind(cbind(x_initial[,1],distance[,ptr]), x_initial[,2]) # isolate the datapoints specified by pointer
      sorted <-x_i[order(x_i[,2]),] # find the kth nearest neighbours in our specified neighbourhood
      npr_nbhd<-sorted[1:k,3] # find the kth nearest neighbours in our specified neighbourhood
      maxim<-max(npr_nbhd) # find the maximum in the densities
      maxim_idx<-which(x_i[,3]==maxim)[1] # find index of maximum in densities
      if (maxim_idx==ptr){ # check if ptr points to itself: if T then:
        mode<-x_initial[maxim_idx,1] # save index of this pointer to find the mode
        ptr_mat[iter, ]<-cbind(cbind(iter,x_initial[iter,1]), mode) # save data
        kill_loop<-1 # kill while loop=> global loop iter++;
      }
      ptr<-maxim_idx # if pointer not point to self then continue while but update ptr
    }
  }
  return(ptr_mat)
}


