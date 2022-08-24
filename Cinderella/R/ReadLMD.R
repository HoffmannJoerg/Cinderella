ReadLMD = function(FileName, FilePath = "", 
                   VarsToCompensate = NULL, tryToCompensate = T){
  # V = ReadLMD(FileName, FilePath)
  # INPUT
  # FileName  
  # FilePath
  # VarsToCompensate    Vector of TRUE and FALSE for each variable in the data. TRUE will be compensated.
  #                       If not given, the function will try to find the right variables itself. Boolean Vector the length of number of variables.
  # tryToCompensate   tries to compensate the PlainData if a compensation matrix can be found
  # Anonymize         tries(!) to remove common keywords containing information about patients. THIS IN NO WAY MAKES DATA SAVE TO GIVE OUT!!
  # OUTPUT
  #  list of:
  #    V$CompensatedData
  #    V$PlainData  Data in a plain matrix format. This is the data as it was found in the lmd/fcs file which might already be compensated
  #    V$RAWfcs    relates to flowCore::read.FCS
  #    V$CompensationMatrix    Compensation Matrix if existing
  #    V$VarNames        Names / Descriptions of each Variable
  #    V$VarIdentifiers  FL Number etc. used by the machine to adress the variable
  
  if (!requireNamespace('flowCore', quietly = TRUE)) {
    message(
      'Subordinate package (flowCore) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      "Subordinate package (flowCore) is missing.
                Please install the package which is defined in 'Suggests'."
    )
    
  }
  
  # Temporarily REMOVED INPUT
  # Anonymize if T, then parts of FCS Data get overwritten by placeholders to remove links to patients
  
  if (nchar(FilePath) > 0) {
    if (substr(FilePath, nchar(FilePath), nchar(FilePath)) != '/')
      FilePath = paste0(FilePath, '/')
  }
  
  frame1 = flowCore::read.FCS(paste0(FilePath, FileName), dataset = 1)
  frame2 = flowCore::read.FCS(paste0(FilePath, FileName), dataset = 2)
  

  data = as.matrix(frame2@exprs)

  # match addresses with description
  AddressToDescr = sapply(1:ncol(as.matrix(frame1@exprs)), function(p)
    c(frame1@description[[paste0("@P", p, "ADDRESS")]],
      frame1@description[[paste0("$P", p, "S")]]))
  # get descriptions for variables
  varDescr = sapply(1:ncol(data), function(p)
    AddressToDescr[2, AddressToDescr[1, ] == frame2@description[[paste0("@P", p, "ADDRESS")]]])
  
  if (!is.null(VarsToCompensate)) {
    if (ncol(data) != length(VarsToCompensate)) {
      warning(
        "ReadLMD: The number of Variables does not match the length of the VarsToCompensate Vector. The
              Data will therefore not be compensated. Please check the variable names in the output of this function."
      )
      tryToCompensate = F
    }
  }
  
  # load spillover matrix
  spill = frame2@description$'$SPILLOVER'
  
  compensation = NULL
  if (!is.null(spill)) {
    for (i in 1:ncol(spill)) {
      spill[i, i] = 1
    }
    compensation = solve(spill)
  }
  
  # search for names
  varNames = sapply(1:ncol(data), function(i)
    ifelse(is.null((a = frame2@description[paste0("$P", i, "N")][[1]])), NA, a))
  
  # fix naming notation
  fixedVarDescr = gsub("_INT_LIN", "", gsub("/", "v", gsub(" ", "_", varDescr)))
  colnames(data) = fixedVarDescr
  
  CompensatedData = NULL
  if (tryToCompensate) {
    if (is.null(VarsToCompensate)) {
      # find Values to compensate
      VarsToCompensate = grepl("FL[[:digit:]]", varNames)
    }
    
    print(paste(
      "The following variables WILL be compensated: ",
      paste(fixedVarDescr[VarsToCompensate], collapse = ", ")
    ))
    print(paste(
      "The following variables WILL NOT be compensated: ",
      paste(fixedVarDescr[!VarsToCompensate], collapse = ", ")
    ))
    
    colnames(spill) = fixedVarDescr[VarsToCompensate]
    rownames(spill) = fixedVarDescr[VarsToCompensate]
    
    CompensatedData = applyCompensationMatrix(data, compensation, VarsToCompensate)
  }
  
  return(
    list(
      CompensatedData = CompensatedData,
      PlainData = data,
      RAWfcs = frame1,
      CompensationMatrix = compensation,
      VarNames = fixedVarDescr,
      VarIdentifiers = varNames
    )
  )
}
