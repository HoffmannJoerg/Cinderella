applyCompensationMatrix = function(Data, Compensation, Columns){
  DataToCompensate = Data[,Columns]
  CompensatedData = DataToCompensate %*% Compensation
  Data[,Columns] = CompensatedData
  return(Data)
}