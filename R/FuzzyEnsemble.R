FuzzyEnsemble=function(TrainData,TargetToTrain,TestData,TargetToTest,Operator=min,CapMin=0,CapMax=100,PlotIt=FALSE){
  
  message("FuzzyEnsemble: Part 1 is computing...")
  res_brnn=Cinderella::MLregressor(TrainData,TargetToTrain,TestData,PlotIt = F,Method = "brnn")
  message("FuzzyEnsemble: Part 2 is computing...")
  res_gbm=Cinderella::MLregressor(TrainData,TargetToTrain,TestData,PlotIt = F,Method = "gbm")
  message("FuzzyEnsemble: Part 3 is computing...")
  res_enet=Cinderella::MLregressor(TrainData,TargetToTrain,TestData,PlotIt = F,Method = "enet")
  message("FuzzyEnsemble: Part 4 is computing...")
  res_bagEarth=Cinderella::MLregressor(TrainData,TargetToTrain,TestData,PlotIt = F,Method = "bagEarth")
  Model=list(BRNN=res_brnn,GBM=res_gbm,Enet=res_enet,bagEarth=res_bagEarth)
  
  
  CapMax=CapMax+1
  CapMin=CapMin-1
  #Training
  PredictedTraining1=res_brnn$PredictedTraining
  PredictedTraining2=res_gbm$PredictedTraining
  PredictedTraining3=res_enet$PredictedTraining
  PredictedTraining4=res_bagEarth$PredictedTraining

  PredictedTraining1[PredictedTraining1>100]=CapMax
  PredictedTraining2[PredictedTraining2>100]=CapMax
  PredictedTraining3[PredictedTraining3>100]=CapMax
  PredictedTraining4[PredictedTraining4>100]=CapMax
  
  PredictedTraining1[PredictedTraining1<=0]=CapMin
  PredictedTraining2[PredictedTraining2<=0]=CapMin
  PredictedTraining3[PredictedTraining3<=0]=CapMin
  PredictedTraining4[PredictedTraining4<=0]=CapMin
  
  PredictedTrainingMatrix=cbind(PredictedTraining1,PredictedTraining2,PredictedTraining3,PredictedTraining4)
  colnames(PredictedTrainingMatrix)=names(Model)
  
  #Test
  Target1=res_brnn$PredictedTarget
  Target2=res_gbm$PredictedTarget
  Target3=res_enet$PredictedTarget
  Target4=res_bagEarth$PredictedTarget
  
  Target1[Target1>100]=CapMax
  Target2[Target2>100]=CapMax
  Target3[Target3>100]=CapMax
  Target4[Target4>100]=CapMax

  Target1[Target1<=0]=CapMin
  Target2[Target2<=0]=CapMin
  Target3[Target3<=0]=CapMin
  Target4[Target4<=0]=CapMin
  
  TargetMatrix=cbind(Target1,Target2,Target3,Target4)
  colnames(TargetMatrix)=names(Model)
  
  ##Ensemble
  FuzzyAggr=function(x,Operator,CapMin,CapMax,...){
    x=x[x!=CapMin]
    if(length(x)==0) return(CapMin+1)
    
    x=x[x!=CapMax]
    if(length(x)==0) return(CapMax-1)
    
    return(Operator(x,...))
  }
  PredictedEnsembleTraining=apply(PredictedTrainingMatrix, 1, FuzzyAggr,Operator,CapMin,CapMax,na.rm=TRUE)
  PredictedEnsembleTarget=apply(TargetMatrix, 1, FuzzyAggr,Operator,CapMin,CapMax,na.rm=TRUE)
    
  # PredictedEnsembleTarget=mapply(function(x,y,a,b){
  #   if(is.null(x)) x=rep(NaN,length(x))
  #   if(is.null(y)) y=rep(NaN,length(y))
  #   if(is.null(b)) b=rep(NaN,length(b))
  #   if(is.null(a)) a=rep(NaN,length(a))
  #   
  #   x[x>0]=NaN
  #   y[y>0]=NaN
  #   b[b>0]=NaN
  #   a[a>0]=NaN
  #   
  #   x[x<=0]=NaN
  #   y[y<=0]=NaN
  #   b[b<=0]=NaN
  #   a[a<=0]=NaN
  #   
  #   return(apply(cbind(x,y,b,a), 1, min,na.rm=TRUE))
  # },Target1,Target2,Target3,Target4,SIMPLIFY = F)
  # 
  # #Capped
  # PredictedEnsembleTarget[PredictedEnsembleTarget>100]=100
  # PredictedEnsembleTarget[PredictedEnsembleTarget<0]=0
  
  QMs=vector(mode="numeric",length = 5)*NaN
  names(QMs)=c("SSE","Spearman","RMSE","R2","adjustedR2")
  
  if(!missing(TargetToTest)){
    if(is.vector(TargetToTest) & is.numeric(TargetToTest)) {
      
    }else{
      warning("TargetToTest is not anumeric vector trying to convert...")
      TargetToTest=as.numeric(TargetToTest)
    }
    
    QMs=RegressionQualityMeasures(Target=TargetToTest,Predicted=PredictedEnsembleTarget,NoFeaturesData=ncol(TestData),PlotIt=PlotIt)
  }#not missing TargetToTest
  
  return(list(PredictedTarget=PredictedEnsembleTarget,QMs=QMs,PredictedTraining=PredictedEnsembleTraining,Model=list(Model=Model,SinglePredictionsTest=TargetMatrix,SinglePredictionsTraining=PredictedTrainingMatrix)))
}