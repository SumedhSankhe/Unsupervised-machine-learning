euclidean <- function(p1,p2){
  # function caluclates the euclidean distance
  # args:
  #   p1  : can take any data structure coercible to a matrix
  #   p2  : can take any data structure coercible to a matrix
  # return:
  #   a distance matrix of the calculate distances
  
  if(!"matrix" %in% class(p1))
    p1 <- as.matrix(p1)
  
  if(!"matrix" %in% class(p2))
    p2 <- as.matrix(p2)
  
  
  distDT <- matrix(NA_real_, nrow = dim(p1)[1], ncol = dim(p2)[1])
  
  for (i in 1:nrow(p2)) {
    distDT[,i] <- sqrt(rowSums(t(t(p1)-p2[i,])^2))
  }
  return(distDT)
}

matequal <- function(x, y){
  # Compares two matrices and gives a logical answer T/F
  # args :
  #   x : A matrix
  #   y : A matrix
  # returns
  #   A logical value of either TRUE or FALSE
  
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

centroid_generator <- function(dt, k){
  # This function generates random centroids for the given data
  # args  :
  #   dt  : a data frame type object
  #   k   : number of centroids to generated
  # returns
  #   A data frame of all the generated centroids
  
  dat <- sapply(dt, function(x){
    runif(k, min(x), max(x))
  })
  if (k == 1)
    return(as.data.frame(t(dat)))
  else
    return(as.data.frame(dat))
}

get_metrics <- function(x,pred){
  # This function calculates the metrics using the reticulate library
  # this library enables us to connect to python and use all its modules
  #
  # args:
  #   x : This is the dataframe of the original data with labels
  # pred: This contains the predicted labels by our model
  #
  # return:
  #   A list of metrics that are calculated using scikit-learn
  library(reticulate)
  
  sklearn.metrics <- import("sklearn.metrics")
  nmi <- sklearn.metrics$normalized_mutual_info_score(x$V3,pred)
  if(length(unique(pred)) >= 2){
    ch <- sklearn.metrics$calinski_harabaz_score(x[,.(V1,V2)],pred)
    sc <- sklearn.metrics$silhouette_score(x[,.(V1,V2)],pred)
  }else{
    ch <- NA_real_
    sc <- NA_real_
  }
  
  return(list(NMI = nmi, CH = ch, SC = sc))
}