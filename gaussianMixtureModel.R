gmm <- function(data, k, init = NA, mu = NA, cov = NA,
                maxiters = 1000, restarts = 10, del = 0.00001,
                initFunc = kMeans){
  
  # This function cluster the data using th gaussian mixture models algorithm
  #
  # args:
  #   data    : An unlabelled dataframe
  #   k       : Number clusters to initialize to
  #   mu      : mean value if to be provided, calculated if not provided
  #   cov     : A covariance matrix if to be provided, assumes a diagonal covariance
  #             if not provided
  #   restarts: Number of restarts to consider defaults at 10
  #   maxiters: Number of iterations to check convergence defaults at 20, lesser value
  #             might not result in convergence
  #   initFunc: Initial cluster generation, defaults to the kmeans algorithm and
  #             iterates using the EM-Algorithm from found clusters
  #
  #
  # returns:
  #  A names list of the various parameters found during the algorithm namely
  #  the log likelihood estimation, BIC and the found clusters
  
  mvpdf <- function(x, mu, sigma) {
    # This function calculates the pdf for the give dataset
    #
    # args:
    #   x   : An unlabelled dataframe
    #   mu  : Mean of the dimensions in the data
    # sigma : Standard deviation of the dimensions in the data
    #
    #
    # returns:
    #   A matrix of the calculated pdf's
    if (det(sigma) == 0) {
      warning("Determinant is equal to 0.")
    }
    print(det(sigma))
    apply(x, 1, function(x) {
      exp(-(1/2) * (t(x) - mu) %*% MASS::ginv(sigma) %*% 
            t(t(x) - mu))/sqrt(det(2 * pi * sigma))
    })
  }
  
  seqK <- seq(1,k,1)
  # initialize clusters using the kmeans 
  if(is.na(init)) 
    init <- matrix(initFunc(data, k = k)$clusters)
  
  if(is.na(mu)){
    mu <- split(data, init)
    mu <- t(sapply(mu, colMeans))
  }
  
  if(is.na(cov))
    # Covariance Matrix for each initial class.
    cov <- lapply(seqK, function(x) diag(ncol(data)))
  
  # Mixing Components
  a <- runif(k)
  a <- a/sum(a)
  
  muHist <- vector(mode = "list")
  covHist <- vector(mode = "list")
  logLikeHist <- vector(mode = "list")
  
  for(j in 1:restarts){
    cat("Restart",j,"\n")
    if (j > 1){
      mu = muHist[[j-1]]
      cov = covHist[[j-1]]
    }
    logLikeIter <- vector()
    for (i in 1:maxiters) {
      cat("Starting Iter",i,"\n")
      # Calculate PDF with class means and covariances.
      b <- simplify2array(lapply(seqK, function(i){
        mvpdf(data,mu[i,],cov[[i]])
      }))
      # Expectation Step for each class.
      d <- simplify2array(lapply(seqK, function(i){
        a[i]*b[,i]/rowSums(t((t(b) * a)))
      }))
      # Choose the highest rowwise probability
      eK <- factor(apply(d, 1, which.max))
      # Total Responsibility
      mc <- colSums(d)
      # Update Mixing Components.
      a <- mc/NROW(data)
      # Update our Means
      mu <- do.call(rbind,(lapply(seqK, function(i){
        colSums(data * d[,i]) * 1/mc[i]
      })))
      cov <- lapply(seqK, function(i){
        cov[[i]] <- t(d[,i] * t(apply(data,1,function(x) x - mu[i, ]))) %*% 
          (d[,i] * t(apply(data, 1, function(x) x - mu[i,]))) * 1/mc[i]
      })
      # Compute the sum of the mixture densities, take the log, and add the
      # column vector.
      loglik <- sum(log(apply(t(t(b) * a), 1, sum)))
      logLikeIter[i] <- loglik
      
      if (i > 1){
        if ((loglik -logLikeIter[i-1]) < del){
          logLikeHist[[j]] <- loglik
          muHist[[j]] <- mu
          covHist[[j]] <- cov
          break("Inner Break Condition Achieved")
        }
      }
    }
    if(j>1)
      if(logLikeHist[[j-1]]==logLikeHist[[j]])
        break
  }
  return(list(data = data, k = k, clusters = eK, 
              mu = mu, logLike = logLikeHist[[j]], init = init))
}