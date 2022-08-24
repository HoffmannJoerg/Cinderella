SplitPercentage=function(Data,Target,Percentage){
  nd=dim(Data)
  n=nd[1]
  d=nd[2]
  if(d<2) stop("Data has to be a matrix.")
  if(n!=length(Target)) stop("Length of Target has to be equal to number of rows of Data")
  if(!is.matrix(Data)){
    warning("Data is not matrix trying to convert...")
    Data=as.matrix(Data)
  }
  if(is.vector(Target) & is.numeric(Target)) {
    
  }else{
    warning("Target is not anumeric vector trying to convert...")
    Target=as.numeric(Target)
  }
  
  if(Percentage>99.99) stop(" Percentage cannot be higher than 99.99")
  if(Percentage<=0) stop(" Percentage cannot be zero or lower")
  
  if(Percentage>1) Percentage=Percentage/100

  indtrain=sample(1:nrow(Data),round(Percentage*nrow(Data),0))
  Train=Data[indtrain,, drop = FALSE]
  indtest=setdiff(1:nrow(Data),indtrain)
  Test=Data[indtest,, drop = FALSE]
  TargetTrain=Target[indtrain]
  TargetTest=Target[indtest]
  
  return(list(Train=Train,Test=Test,TargetTrain=TargetTrain,TargetTest=TargetTest,indtrain=indtrain,indtest=indtest)) 
}