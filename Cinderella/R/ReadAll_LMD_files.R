ReadAll_LMD_files=function(Directory=getwd(),Extension=".LMD",cl){
  if (missing(cl)) {
    if (!requireNamespace('parallel', quietly = TRUE)) {
      message(
        'Subordinate package (parallel) is missing. No computations are performed.
              Please install the package which is defined in "Suggests".'
      )
      return(
        "Subordinate package (parallel) is missing.
                  Please install the package which is defined in 'Suggests'."
        
      )
    }
    cl=parallel::detectCores() - 1

  }
  stopclust=TRUE
  
  if (!is.null(cl)) {
    if (!requireNamespace('parallel', quietly = TRUE)) {
      message(
        'Subordinate package (parallel) is missing. No computations are performed.
              Please install the package which is defined in "Suggests".'
      )
      return(
        "Subordinate package (parallel) is missing.
                  Please install the package which is defined in 'Suggests'."
        
      )
    }
    
    if(!is.list(cl))
      cl = parallel::makeCluster(cl)
    else
      stopclust=FALSE
  }else{
    stopclust=FALSE
  }
  
  names = list.files(path = Directory, pattern = Extension)
  
  if (!is.null(cl)) {
    DataV = parallel::parLapply(cl, names, function(x, y) {
      #try({
      x = Cinderella::ReadLMD(x, y)$CompensatedData#})
      return(x)
    }, Directory)
  } else{
    DataV = lapply(names, function(x, y) {
      #try({
      x = ReadLMD(x, y)$CompensatedData#})
      return(x)
    }, Directory)
    
  }
  
  if (isTRUE(stopclust)) {
    parallel::stopCluster(cl)
  }
  filenames=gsub(".LMD","",names)
  filenames=gsub(" ","_",filenames)

  names(DataV)=filenames
  return(DataV)
}