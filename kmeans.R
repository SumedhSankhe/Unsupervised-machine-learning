kMeans <- function(rawData, k, init = NA, restarts = 10, maxiters = 20,
                   func = euclidean){
  #this function calculates the kmean neighborhood
  # args:
  #   data      : the data.table provided to this function
  #   k         : a numeric value for the number of partitions
  #   init      : inital centroid if provided else NA
  #   restarts  : number of restarts, default 10
  #   iters     : max number of iterations in each start, default 20
  #   func      : we can pass the required distance  function here
  # returns:
  
  if(!is.numeric(k))
    stop("Number of clusters 'k' should be a whole number")
  
  if(!is.na(init)){
    if(!class(init) %in% c("data.frame","data.table"))
      stop("Initial Centroids need to passed a data.frame or data.table")
    
    if(nrow(init) != k)
      stop("Initial Centroids should be equal to number of cluster")
  }
  
  library(data.table)
  
  rawData <- as.data.table(rawData)
  
  centroidRestarts <- vector(restarts, mode = "list")
  withinssDT <- matrix(ncol = 2, nrow = restarts)
  
  for (r in 1:restarts) {
    # random sample of k centriods if not provided
    data <- rawData
    
    if(is.na(init))
      centroids <- centroid_generator(data, k)
    
    # initialize a list that can store centroids
    centroidHist <- vector(maxiters, mode = "list")
    # initialize a list that will store the clusters
    assignedCluster <- vector(maxiters, mode = "list")
    
    for (i in 1:maxiters) {
      distToCenter <- func(data, centroids)
      clusters <- apply(distToCenter, 1, which.min)
      centroids <- apply(data, 2, tapply, clusters, mean)
      if(k > 1){
        centroids <- data.frame(centroids)
      }else {
        centroids <- data.frame(t(centroids))
      }
      centroidHist[[i]] <- centroids
      assignedCluster[[i]] <- clusters
      #i = i +1
      # check for condition only when number of iterations is 
      # more than 1
      # if the centriods are exactly equal to the previous run
      # the loop stops
      if(i > 1 )
        if(matequal(centroids, centroidHist[[i-1]]))
          break
    }
    
    data$cluster <- assignedCluster[[i]]
    meanDT <- data.table(aggregate(subset(data,select = -cluster),
                                   list(data$cluster),mean))
    
    # calculates the withiness of the clusters i.e. within clusters distance
    withinss <- vector()
    for(ks in data[,unique(cluster)]){
      p1 <- data[cluster == ks]
      p2 <- meanDT[Group.1 == ks]
      if(nrow(p1) != 0 & nrow(p2) != 0){
        p1[,cluster := NULL]
        p2[,Group.1 := NULL]
        withinss[ks] <- sum(func(p1,p2))
      }
    }
    
    tot_withiness <- sum(withinss)
    
    centroidRestarts[[r]] <- list(centroid = centroidHist[[i]],
                                  clusters = assignedCluster[[i]],
                                  withinss = withinss,
                                  tot_withiness = tot_withiness,
                                  iter = i,
                                  data = data)
    
    withinssDT[r,] <- c(tot_withiness, r)
  }
  
  iter <- data.table(withinssDT)[which.min(V1),V2]
  result <- centroidRestarts[[iter]]
  
  return(list(centroid = result$centroid, 
              clusters = result$clusters,
              data = result$data,
              withinss = result$withinss,
              tot_withines = result$tot_withines,
              k = k))
}