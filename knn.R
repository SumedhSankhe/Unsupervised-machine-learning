algorithm_knn <- function(trainData, testData, cl, k){
  # This function producs a knn output a vector of predicted class
  #
  # args:
  #   trainData: Train partition
  #   testData: Test partition
  #         cl: train variable 
  #         k: number of nearest neighbors
  #
  # returns:
  #   A vector with predicted class for the test data
  
  # if dimensions of the train and test data are not equal stop algorithm
  if(dim(testData)[2] != dim(trainData)[2])
    stop("Dimensions of Test and Train not equal")
  
  # knn is a greedy alf
  f <- apply(testData, 1, function(x){
    dt <- apply(trainData, 1,function(y){
      dist(rbind(x,y))[1] # distance calculation using base R function 
    })                    # euclidean distance considered
    DT <- data.frame(d = dt, ks = cl)
    DT <- DT %>% arrange(d) %>% head(k) # consider only k neighbors
    DT <- data.frame(table(DT$ks))
    DT[which.max(DT$Freq),]$Var1 # determine class based on max freq of neighbors
  })
  return(f)
}
