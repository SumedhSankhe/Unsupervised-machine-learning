dbScan <- function (data, eps, minPts, func){
  # This function implements the DBSCAN clustering algorithm
  # args:
  #   data  : take any dataframe or similar structure coercible to matrix
  #   eps   : take the minimum radius in which to look for datapoints
  #   minPts: number of minimum data points to look for in the neigborhood
  #   func  : takes a function that can calculate distance between two points
  #
  # returns:
  #   A named list of the calculated clusters as a vector, as well as
  #   the given parameters, also returns a data.frame with the assigned clusters
  
  
  # convert data to a matrix
  data <- as.matrix(data)
  # get the number of rows in the dataset
  n <- nrow(data)
  
  #initialize empty vectors to keep track of the clusters
  classn <- integer(n)
  cv <- classn
  cn <- 0
  
  for (i in 1:n) {
    noClass <- (1:n)[cv < 1]
    if (cv[i] == 0){
      # compute distance to every point for a give point and keep only the points 
      # required by the eps threshold
      rel <- noClass[as.vector(func(data[i, , drop = FALSE],
                                    data[noClass, , drop = FALSE])) <= eps]
      # if the length of the points that are found is less than the ones required
      # then classify them as noise
      if (length(rel) + classn[i] < minPts) 
        cv[i] <- -1
      # else process them furhter and find other points in the eps the distance from them
      else {
        cn <- cn + 1
        cv[i] <- cn
        rel <- setdiff(rel, i)
        noClass <- setdiff(noClass, i)
        classn[rel] <- classn[rel] + 1
        # loop while all the points that are density reachables from the first point are
        # exhausted/ assigned to a particular cluster.
        while (length(rel)) {
          cv[rel] <- cn
          ap <- rel
          rel <- integer()
          # repeat the process of finding density reachable points for the new found
          # list and iterate 
          for (k in seq(along = ap)) {
            j <- ap[k]
            jRel <- noClass[as.vector(func(data[j, , drop = FALSE],
                                           data[noClass, , drop = FALSE])) <= eps]
            
            if (length(jRel) + classn[j] >= minPts) {
              cv[jRel[cv[jRel] < 0]] <- cn
              rel <- union(rel, jRel[cv[jRel] == 0])
            }
            classn[jRel] <- classn[jRel] + 1
            noClass <- setdiff(noClass, j)
          }
        }
      }
    }
    # break the loop when there are no point remaining that need to be assigned
    if (!length(noClass)) 
      break
  }
  # if any points that land in cluster -1 are unclassified or not density reachable from
  # any of the other points in the data set
  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  
  data <- as.data.table(data)
  data$cluster <- as.factor(cv)
  
  return(list(cluster = cv, eps = eps, minPts = minPts, data = data))
}