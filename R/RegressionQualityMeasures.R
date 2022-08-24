RegressionQualityMeasures=function(Target,Predicted,NoFeaturesData,PlotIt=TRUE){
  
  QMs=vector(mode="numeric",length = 5)*NaN
  names(QMs)=c("SSE","Spearman","RMSE","R2","adjustedR2")
  
  n2=length(Target)
  if(missing(NoFeaturesData)){
    NoFeaturesData=n2-2 #worst case
  }
  if(length(Predicted)!=n2) stop("RegressionQualityMeasures: Length of Predicted has to be equal to the length of Target")
  
  QMs[1]=sum((Target-Predicted)^2,na.rm = T)
  QMs[2]=cor(Target,Predicted,method = "spearman")
  QMs[3]=sqrt(QMs[2]/n2)
  m=mean(Target,na.rm = T)
  SStot=sum((Target-m)^2,na.rm = T)
  #Coefficient of determination
  QMs[4]=  1-(QMs[1]/SStot)
  if((n2-2)<(NoFeaturesData)){
    warning("RegressionQualityMeasures: Usual adjusted R2 is not clearly defined in the case of sample size smaller than number of variables. Limiting the number of variables for computation.")
    NoFeaturesData=n2-2
  } 
  QMs[5]= 1-((QMs[1]/(n2-NoFeaturesData-1))/(SStot/(n2-1))) #vegan::RsquareAdj() #1-(1-QMs[4]^2)*((n2-1)/(n2-NoFeaturesData-1)) 
  if(isTRUE(PlotIt)){
    if (!requireNamespace('DataVisualizations')) {
      message(
        'RegressionQualityMeasures: Subordinate DataVisualizations package is missing. No Plotting are performed.
            Please install the package which is defined in "Suggests".'
      )
    }
    else{
      DataVisualizations::MAplot(Target,Predicted,islog = F,main = "Bland-Altman plot")
    }
  }#PlotIt
  return(QMs)
}