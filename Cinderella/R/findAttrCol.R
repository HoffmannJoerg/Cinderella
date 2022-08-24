findAttrCol <- function(AttrName,Header,Data){

# function [AttrCol] = findAttrCol(AttrName,Header,Data);
# [AttrCol] = findAttrCol(AttrName,Header,Data);
# Find attribute column of data matrix by name

# INPUT
# AttrName  attribute name, trailing spaces can be omitted
# Header  array with column names
# Data      data matrix

# OUTPUT
# AttrCol   attribute column vektor

  
  i <- which(Header == AttrName)
  if (length(i) == 1) {
    AttrCol = Data[, i]
  } else {
    AttrCol = NaN
    warning('findAttrCol: No or multiple hits for "', AttrName, '" !')
  } #if (length(i) ==1)

  return(AttrCol)
}
