findAttrInd <- function(AttrName,VarNames){
# AttrInd = findAttrInd(AttrName,VarNames);
# Find attribute column index of data matrix by name

# INPUT
# AttrName  attribute name, trailing spaces can be omitted
# VarNames  array with column names

# OUTPUT
# AttrInd   attribute column index

# [AnzVar,VarNameLength] = size(VarNames);
# AttrNameLength = length(AttrName);
# AttrInd = 0;  % default return
# if AttrNameLength > VarNameLength,  return; end; % AttrNameLength > VarNameLength

if(is.vector(VarNames)){
	VarNames <- as.matrix(VarNames)
}

if(is.matrix(VarNames)){
	x <- dim(VarNames)
	AnzVar <- x[1]
	VarNameLength <- x[2]
}else{
	stop('findAttrInd(): VarNames must be a vector or a matrix!')
}

AttrNameLength <- length(AttrName)
AttrInd <- 0
if(AttrNameLength > VarNameLength){
	return(AttrInd)
}

# for i = 1:size(VarNames,1)
#   if findstr(VarNames(i,:), AttrName) == 1
#     AttrInd = i;
#     return
#   end
# end

for( i in c(1:AnzVar)){
	z <- grep(VarNames[i], AttrName)
	if (length(z)>0){
	  if(z == 1){
		 AttrInd <- i
		 return(AttrInd)
	  }# end if(z == 1)
   }# end length(z)>0
 }# end for

}# end findAttrInd()

