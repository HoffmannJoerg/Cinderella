\name{ReadAll_LMD_files}
\alias{ReadAll_LMD_files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ReadAll LMD files
}
\description{
Reads all LMD files in a directory and gives them back as a list of data matrices.
}
\usage{
ReadAll_LMD_files(Directory = getwd(), Extension = ".LMD", cl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Directory}{
directory in which lmd files are stored
}
  \item{Extension}{
specific extension of files, usually either ".LMD" or ".lmd"
}
  \item{cl}{
if missing, generates one core less than exisits on pc and computes on each core the populations
if \code{NULL} uses one core
otherwise specify manually using cl=parallel::makeCluster(...)
}
}
\details{
data is compensated, details see \code{\link{ReadLMD}}. Output list is named by file names.
}
\value{
named list [1:m] of numerical data matrices [1:n?,1:d?] of n cases and d features, n and d can vary in each list element
}
\author{
Michael Thrun
}

\seealso{
\code{\link{ReadLMD}}
}
\examples{
#access to data has to be given by Dr. Joerg Hoffmann.
\dontrun{
library(Cinderella)

Disk="E"
path=ReDi("BlutVsKM_Cinderella2020/90RawData/Validierungsproben",Disk)
numberofCores=10
cl=parallel::makeCluster(numberofCores)
library(Cinderella)
DataV=ReadAll_LMD_files(path,cl = cl)
names(DataV)
}
}
