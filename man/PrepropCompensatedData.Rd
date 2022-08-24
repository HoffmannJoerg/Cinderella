\name{PrepropCompensatedData}
\alias{PrepropCompensatedData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PrepropCompensatedData
}
\description{
Simple Preprocessing steps for feature standardization and normalization.
}
\usage{
PrepropCompensatedData(DataV, ExactMarkers = "FS",

MarkerNames = c("CD7", "CD13", "CD33", "CD34",

"CD45", "CD56", "CD117", "HLA", "SS"), cl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataV}{
[1:m] list if numerical data matrices [1:n?,1:d?] of n cases and d features, n and d can vary in each list element
}
  \item{ExactMarkers}{
character vector of markets for which exact matches should be searched in data
}
  \item{MarkerNames}{
character vector of markets for which substrings should be search in data
}
  \item{cl}{
if missing, generates one core less than exisits on pc and computes on each core the populations
if \code{NULL} uses one core
otherwise specify manually using cl=parallel::makeCluster(...)
}
}
\details{
each numerical matrix in the output list is exactl nameed by the substrings of \code{MarkerNames} and the strings of \code{ExactMarkers}. All other features are disregarded. Not found markers are ignored.
Data is given back as signed log, please see \code{\link[DataVisualizations]{SignedLog}} for details.
}
\value{
[1:m] list if numerical data matrices [1:n?,1:d?] of n cases and d features, n and d can vary in each list element.
}
\author{
Michael Thrun
}

\seealso{
\code{\link[DataVisualizations]{SignedLog}}
}

\examples{
\dontrun{
library(Cinderella)

Disk="E"
path=ReDi("BlutVsKM_Cinderella2020/90RawData/Validierungsproben",Disk)
numberofCores=10
cl=parallel::makeCluster(numberofCores)
library(Cinderella)
DataV=ReadAll_LMD_files(path,cl = cl)
names(DataV)
TransV=PrepropCompensatedData(DataV,cl = cl)
#names(TransV)
}
}
