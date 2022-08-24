MLregressor=function(TrainData,TargetToTrain,TestData,TargetToTest,Method="brnn",PrintIt=FALSE,PlotIt=FALSE,SkipCheck=TRUE,...){
  
  if(!is.matrix(TrainData)){
    warning("TrainData is not matrix trying to convert...")
    TrainData=as.matrix(TrainData)
  }
  nd=dim(TrainData)
  n=nd[1]
  d=nd[2]
  
  if(d<1) stop("TrainData has to be a matrix.")
  if(n!=length(TargetToTrain)) stop("Length of TargetToTrain has to be equal to number of rows of TrainData")
  if(is.vector(TargetToTrain) & is.numeric(TargetToTrain)) {
    
  }else{
    warning("TargetToTrain is not anumeric vector trying to convert...")
    TargetToTrain=as.numeric(TargetToTrain)
  }
  
  if(!missing(TestData)){
  if(!is.matrix(TestData)){
    warning("TestData is not matrix trying to convert...")
    TestData=as.matrix(TestData)
  }
  
  nd2=dim(TestData)
  n2=nd2[1]
  d2=nd2[2]
  
  if(d!=d2) stop("TrainData and TestData differ in the number of columns")
  }
  
  if (!requireNamespace('caret',quietly = TRUE)) {
    message(
      'Subordinate clustering package is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    if(!missing(TestData))
      return(
        list(
          TargetToTest = rep(NaN, nrow(TestData)),
          Object = "Subordinate clustering package is missing.
                  Please install the package which is defined in 'Suggests'."
        )
      )
    else
      return(
        list(
          TargetToTest = rep(NaN, 1),
          Object = "Subordinate clustering package is missing.
                  Please install the package which is defined in 'Suggests'."
        )
      )
  }
  if(isTRUE(PrintIt)){
    message('Possible Model to select from:')
    print(names(caret::getModelInfo()))
  }
  if(Method=="enet")
      lib="elasticnet"
  else if(Method=="bagEarth")
    lib="earth"
  else
    lib=Method
  
  if(isFALSE(SkipCheck))
    caret::checkInstall(lib)
  
  QMs=vector(mode="numeric",length = 5)*NaN
  names(QMs)=c("SSE","Spearman","RMSE","R2","adjustedR2")
  Model=NULL
  PredictedTraining=rep(NaN,nrow(TrainData))
  try({
  Model=caret::train(TrainData,TargetToTrain,method=Method,...)
  PredictedTraining=predict(Model)
  })
  
  if(!missing(TestData)){
    
    PredictedTarget=rep(NaN,nrow(TestData))
    try({
    PredictedTarget=caret::predict.train(object = Model,newdata = TestData)#caret::predict.train(models = Model,testX = TestData)
    })
  }else{
    PredictedTarget=NULL
  }

  
  if(!missing(TargetToTest)){
    if(is.vector(TargetToTest) & is.numeric(TargetToTest)) {
      
    }else{
      warning("TargetToTest is not anumeric vector trying to convert...")
      TargetToTest=as.numeric(TargetToTest)
    }

    QMs=RegressionQualityMeasures(Target=TargetToTest,Predicted=PredictedTarget,NoFeaturesData=d2,PlotIt=PlotIt)
  }#not missing TargetToTest

  return(list(PredictedTarget=PredictedTarget,QMs=QMs,Model=Model,PredictedTraining=PredictedTraining))
}