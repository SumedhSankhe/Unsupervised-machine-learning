myReadLines <- function(fname) {
  # Readline function
  # args : fname -- path of the file to be read in
  #
  # returns : a large character vector 
  # pros : considerably faster than other read functions
  
  s <-  file.info(fname)$size 
  buf <-  readChar(fname, s, useBytes=T)
  strsplit(buf, "\n", fixed=T, useBytes=T)[[1]]
}

dataTransform <- function(fname){
  # Datatransform function transforms the data into a more readable wide
  # format
  #
  # args : fname --- path of the files to be read in 
  #
  # returns: a transformed data in the data.table format, also save the data 
  #          as a csv file to read easily into sparklyr
  # cons : tends to be a bit slow on regular expressions
  
  st <- Sys.time()
  data <- data.table(myReadLines(fname = fname))
  cat('Data read in', Sys.time()-st,'... \n')
  # select regular expression filter using file type
  st <- Sys.time()
  if(grepl("q2", fname)){
    regx <- "#\\* |#\\@ |#t |#c |#index |#% "
    data[,':='(type = gsub(" .*", "", V1))]
    d <- 'q2'
  }else{
    regx <- "#\\*|#\\@|#t|#c|#index|#%|#\\!"
    data[,type := ifelse(grepl("#\\*",V1),"paperTitle",
                         ifelse(grepl("#\\@",V1),"author",
                                ifelse(grepl("#t",V1),"year",
                                       ifelse(grepl("#c",V1),"pubVenue",
                                              ifelse(grepl("#index",V1), "index",
                                                     ifelse(grepl("#%",V1),"ref",
                                                            ifelse(grepl("#\\!",V1),
                                                                   "",'extra')))))))]
    d <- 'q4'
  }
  
  data[,':='(data = gsub(regx, "", V1))]
  
  data[, id := cumsum(!nzchar(type))]
  data <- data[nzchar(type)]
  
  transformed <- dcast.data.table(data, id~type,
                                  fun.aggregate = function(x) paste(x, collapse = ","),
                                  value.var = "data")
  
  rm(data)
  gc()
  if(grepl("q4",fname)){
    transformed[,':='(index = gsub("\r","",index),
                      paperTitle = gsub("\r","", paperTitle),
                      pubVenue = gsub("\r","",pubVenue),
                      year = gsub("\r","", year),
                      author = gsub("\r","", author),
                      ref = gsub("\r","", ref))]
    transformed[,extra := NULL]
  }
  
  if(grepl("q2", fname)){
    setnames(transformed,c("id", "#%", "#*", "#@", "#c", "#index", "#t"),
             c("id", "ref", "paperTitle", "author", "pubVenue", "index", "year"))
    transformed[,':='(author = strsplit(author,";"))]
  } else{
    transformed[,':='(author = strsplit(author,","))]
  }
  
  transformed[ref == "", ref := NA_real_]
  
  transformed[ref == " ", ref := NA_real_]
  
  transformed %>% unnest(author) -> transformed
  transformed[, ref := strsplit(ref,",")]
  
  transformed %>% unnest(ref)-> transformed
  transformed %>% unique()-> transformed
  transformed[,id := NULL]
  
  
  cat('Data Transformed in',Sys.time()-st,'\n')
  write.csv(transformed, paste0("transformed_data_", d, ".csv"), row.names = FALSE)
  
  cat('Data Saved')
  return(transformed)
}