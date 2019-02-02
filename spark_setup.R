setup <- function(pkgs, mem = "8G"){
  # This function sets up the spark configuration that is required
  # if you are running this locally please check if you support the
  # below configuration before running it
  if(!is.character(mem))
    stop(cat(mem," is not of class character: enter '16G' if you want to use more memory"))
  
  lapply(pkgs, function(x){
    if(!require(x, character.only = T))
      install.packages(x)
    library(x, character.only = T)
    if(x %in% "sparklyr")
      spark_install()
  })
  
  if("sparklyr" %in% pkgs){
    conf <- spark_config()
    conf$`sparklyr.shell.driver-memory` <- mem  # use it if you have memory
    conf$spark.memory.fraction <- 0.9 # set fraction of memory for overhead
    sc <- spark_connect(master = "local", config = conf, version = "2.3.0")
    return(sc)
  }
}

# setup spark connection
sc <- setup(pkgs = pkgs, mem ="16G")