PrepropCompensatedData=function(DataV,ExactMarkers="FS",MarkerNames=c('CD7','CD13','CD33','CD34','CD45','CD56','CD117','HLA','SS'),cl){
  
  if(missing(cl)){
    if (!requireNamespace('parallel',quietly = TRUE)) {
      message(
        'Subordinate package (parallel) is missing. No computations are performed.
              Please install the package which is defined in "Suggests".'
      )
      return(
        
        "Subordinate package (parallel) is missing.
                  Please install the package which is defined in 'Suggests'."
        
      )
    }
    cl=parallel::detectCores()-1
  }
  stopclust=TRUE
  
  if(!is.null(cl)){
    if (!requireNamespace('parallel',quietly = TRUE)) {
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
  if (!requireNamespace('stringr', quietly = TRUE)) {
    message(
      'Subordinate package (stringr) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      "Subordinate package (stringr) is missing.
                Please install the package which is defined in 'Suggests'."
    )
    
  }
  if (!requireNamespace('DataVisualizations', quietly = TRUE)) {
    message(
      'Subordinate package (DataVisualizations) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      "Subordinate package (DataVisualizations) is missing.
                Please install the package which is defined in 'Suggests'."
    )
    
  }

  SelectColsAndLog=function(Data,ExactMarkers,MarkerNames){
    Data2=Data
    #try({
    ind=c()
    for(i in 1:length(MarkerNames))
      ind=c(ind,which(stringr::str_detect(colnames(Data),MarkerNames[i])))
    
    for(i in 1:length(ExactMarkers))
      ind=c(ind,findAttrInd(ExactMarkers[i],colnames(Data)))
    
    Data2=DataVisualizations::SignedLog(Data[,ind])
    #reihenfolge muss gleich sein
    Data2=Data2[,order(colnames(Data2))]
    
    #for(i in 1:ncol(Data2)){
    #  Data2=Data2[Data2[,i]>=0.1,]
    #}
    # })
    return(Data2)
  }
  #Daten Preprop ----

  ReNameCols=function(Data){
    #try({
    Header=colnames(Data)
    
    keyadd__ende=stringr::str_locate(Header,"_")[,1]-1
    
    keyadd=mapply(function(x,y) substr(x,1,y), Header,keyadd__ende)
    ind=which(is.na(keyadd))
    keyadd[ind]=Header[ind]
    colnames(Data)=keyadd
    
    indcols=order(colnames(Data),decreasing = T)
    indcols[c(2,3)]=indcols[c(3,2)]
    Data=Data[,indcols]
    #})
    return(Data)
  }
  
  FilterAndScale=function(Data){
    
    rescale=function(D,Dmin,Dmax,LO,HI){
      # rescale D from a range to a range
      # INPUT
      # D           a vector of data
      # [Dmin,Dmax] the input range
      # [LO,HI]     the outout range
      #
      # OUTPUT
      # ScaledData   scaled data such that the data in range Dmin,Dmax]
      #              resides now in [LO,HI] 
      INrange = Dmax-Dmin;
      if(INrange <0)   INrange=1;  # do nothing
      OUTrange = HI-LO; 
      if(OUTrange <0) OUTrange=1; # do nothing
      
      ScaledData = LO + (D-Dmin) * OUTrange/INrange;  # scale to [LO,HI] 
      return(ScaledData)
    }
    
    MinFS = 4.75
    MaxFS = 6
    # zulassiger Wertebereich fuer FS
    MinSS = 4
    MaxSS = 6
    
    FS=Data[,"FS"]
    SS=Data[,"SS"]
    
    Ind = which((FS<MaxFS)&(SS<MaxSS)&(FS>MinFS )&(SS>MinSS))
    Data=Data[Ind,]
    
    Data[,"SS"]=rescale(Data[,"SS"],min(Data[,"SS"]),MaxSS,1,5)
    Data[,"FS"]=rescale(Data[,"FS"],min(Data[,"FS"]),MaxFS,1,5)
    
    Data[Data<0]=0
    Data[Data>5]=5
    
    return(Data)
  }
  

  if(!is.null(cl)){
    LOG_DataV=parallel::parLapply(cl,DataV, SelectColsAndLog,ExactMarkers,MarkerNames)
    LOG_DataV2=parallel::parLapply(cl,LOG_DataV,ReNameCols)
    ##special case for HLA
    if(any(MarkerNames=="HLA"))
      LOG_DataV2=parallel::parLapply(cl,LOG_DataV2, function(Data){
        ind=which(stringr::str_detect(colnames(Data),"HLA"))
        colnames(Data)[ind]="HLA_DR"
        return(Data)
      })
    LOG_DataV3=parallel::parLapply(cl,LOG_DataV2,FilterAndScale)
  }else{
  LOG_DataV=lapply(DataV, SelectColsAndLog,ExactMarkers,MarkerNames)
  LOG_DataV2=lapply(LOG_DataV,ReNameCols)
  ##special case for HLA
  if(any(MarkerNames=="HLA"))
      LOG_DataV2=lapply(LOG_DataV2, function(Data){
      ind=which(stringr::str_detect(colnames(Data),"HLA"))
      colnames(Data)[ind]="HLA_DR"
      return(Data)
    })
  LOG_DataV3=lapply(LOG_DataV2,FilterAndScale)
  }
  if (isTRUE(stopclust)) {
    parallel::stopCluster(cl)
  }
  
  if(!is.null(names(DataV))){
    names(LOG_DataV2)=names(DataV)
  }
  return(LOG_DataV3)
}