\name{ReadLMD}
\alias{ReadLMD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReadLMD
}
\description{
Reads and Compensates a LMD file.
}
\usage{
ReadLMD(FileName, FilePath = "",

VarsToCompensate = NULL, tryToCompensate = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
string name of file
}
  \item{FilePath}{
Optional, string, path in which file is stored, otherwise getwd() is used
}
  \item{VarsToCompensate}{
Optional and internal setting, usually not to be changed.
}
  \item{tryToCompensate}{
Optional and internal setting, usually not to be changed.
}
}
\details{
Assumes navios style, meaning that in the lmd file there are two fcs files, one in version 2, one in version 3 and information of both is combined to generate the output of compensated data.
}
\value{
 \item{CompensatedData }{numerical matrix [1:n,1:d] of compensated data}
\item{PlainData }{not compensated raw data}
\item{CompensationMatrix }{matrix used for compensation}
\item{VarNames }{variable names, see fcs file format}
\item{VarIdentifiers }{variable identifies, see fcs file format}

}

\author{
Florian Lerch, Databionics AG
}
\note{
Input and Output was a little bit adjusted by MCT in comparison to \code{ReadFCSNavios}
}



\seealso{
taken from dbt.dataio \code{ReadFCSNavios}}

