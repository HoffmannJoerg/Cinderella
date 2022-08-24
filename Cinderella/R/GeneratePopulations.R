GeneratePopulations=function(DataV,FUN,cl){
  
  if(is.character(FUN))
    FUN=get(FUN)
  
  
  if (!requireNamespace('plyr',quietly = TRUE)) {
    message(
      'Subordinate package (plyr) is missing. No computations are performed.
              Please install the package which is defined in "Suggests".'
    )
    return(
      "Subordinate package (plyr) is missing.
                  Please install the package which is defined in 'Suggests'."
    )
  }
  
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
  
  PrepareD=function(Xnode,Header,sep="."){
    #allowed sep in R for variable names: "_" or "."
    noF=ncol(Xnode)
    noN=nrow(Xnode)
    nnmat=matrix(0,ncol = noF,noF)
    nc=sum(lower.tri(nnmat,diag = F))
    sep=paste0(sep,".")
    if(missing(Header)){
      Header=paste0("C",1:noF)
    }
    veclen=nc
    relcutvalues=c(0.25,1/3,0.5,2/3,0.75,-0.25,-1/3,-0.5,-2/3,-0.75)
    relcutvalues4header=paste0("a",round(relcutvalues,2))
    relcutvalues4header=gsub(pattern = "a-","n",x = relcutvalues4header)
    
    vec =vector(mode="character",veclen*length(relcutvalues))
    Xnew=matrix(0,ncol = nc*length(relcutvalues),nrow = noN)
    k=1
    
    for(i in 1:noF){
      for(j in 1:noF){
        if(i<j){
          Xnew[,k]     =Xnode[,i]+relcutvalues[1] *Xnode[,j]
          vec[k]=stringi::stri_c(Header[i],relcutvalues4header[1],Header[j],sep = sep)
          for(r in 2:length(relcutvalues)){
            Xnew[,k+(r-1)*nc]=Xnode[,i]+relcutvalues[r] *Xnode[,j]
            vec[k+(r-1)*nc]=stringi::stri_c(Header[i],relcutvalues4header[r],Header[j],sep = sep)
          }#end for r in 1: length relcutvalues
          k=k+1
        }
      }
    }
    return(list(Xnew=Xnew,Colnames=vec))
  }

  if(!is.null(cl)){
    out=parallel::parLapply(cl = cl,X = DataV, fun = function(x,FUN,PrepareD) {
      V=PrepareD(x,Header = colnames(x))
      Cols=V$Colnames
      xext=V$Xnew
      colnames(xext)=Cols
      x=cbind(x,xext)
      V=FUN(x,colnames(x))
      PopulationNumber=V$PopulationNumber
      PopNo=sort(unique(PopulationNumber),decreasing = F)
      IndList=lapply(PopNo, function(i,x) which(x==i),PopulationNumber)
      ClassList=lapply(PopNo, function(i,x,RuleCls) unique(RuleCls[which(x==i)]),PopulationNumber,V$RuleCls)
      names(ClassList)=paste0("R",PopNo)
      names(IndList)=paste0("R",PopNo)
      return(list(IndList=IndList,ClassList=ClassList,RuleCls=V$RuleCls,PopulationNumber=PopulationNumber,PopNo=PopNo))
    },FUN,PrepareD)
  }else{
    out=lapply(X = DataV, FUN =  function(x,FUN) {
        V=PrepareD(x,Header = colnames(x))
        Cols=V$Colnames
        xext=V$Xnew
        colnames(xext)=Cols
        x=cbind(x,xext)
      V=FUN(x,colnames(x))
      PopulationNumber=V$PopulationNumber
      PopNo=sort(unique(PopulationNumber),decreasing = F)
      IndList=lapply(PopNo, function(i,x) which(x==i),PopulationNumber)
      
      ClassList=lapply(PopNo, function(i,x,RuleCls) unique(RuleCls[which(x==i)]),PopulationNumber,V$RuleCls)
      names(ClassList)=paste0("R",PopNo)
      names(IndList)=paste0("R",PopNo)
      return(list(IndList=IndList,ClassList=ClassList,RuleCls=V$RuleCls,PopulationNumber=PopulationNumber,PopNo=PopNo))
    },FUN)
  }
  ClassLists=lapply(out, function(x) as.data.frame(x$ClassList))
  ClassMat=plyr::rbind.fill(ClassLists)
  
  PopNoLists=lapply(out, function(x) as.data.frame(x$PopNo))
  PopNoMat=plyr::rbind.fill(PopNoLists)
  
  #View(ClassMat)#check ok
  Cls=as.numeric(ClassMat[1,])
  names(Cls)=colnames(ClassMat)
  
  PopsInds=lapply(out, function(x) x$IndList)
  Pops=mapply(function(x,y){
    namen=names(x)
    Pop=vector(mode="numeric",length = length(namen))
    names(Pop)=namen
    for(i in 1:length(x))
      Pop[i]=list(length(x[[i]])/nrow(y))
    
    return(Pop)
  }  ,PopsInds,DataV,SIMPLIFY = F)
  names(Pops)=names(DataV)
  
  Pops2=lapply(Pops, as.data.frame)
  PopsMat=plyr::rbind.fill(Pops2)
  
  rownames(PopsMat)=names(DataV)
  PopsMat=as.matrix(PopsMat)
  PopsMat[!is.finite(PopsMat)]=0
  
  if (isTRUE(stopclust)) {
    parallel::stopCluster(cl)
  }
  
  return(list(PopsMat=PopsMat,Cls=Cls,OutputFUN=out))
} 
