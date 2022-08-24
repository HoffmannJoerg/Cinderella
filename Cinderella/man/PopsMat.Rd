\name{PopsMat}
\alias{PopsMat}
\docType{data}
\title{
matrix of population frequencies in percent
}
\description{
population frequencies of bological populations for a sample of lmd files
}
\usage{data("PopsMat")}
\format{
  The format is:
 num [1:10, 1:43] 32.32 6.14 10.38 12.54 16.47 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:10] "342620_0-10ml_KMA_MRD1_2020-11-19_001" "345020_0-10ml_KMA_MRD1_2020-11-23_001" "352520_0-10ml_KMA_MRD1_2020-12-01_001" "352520_50%KMA_MRD1_2020-12-01_001" ...
  ..$ : chr [1:43] "R1" "R2" "R3" "R4" ...
}
\details{
each line defines the populatons R1 to Rn of one lmd file. These populations were extracted from ALPODSclassifier.R which was based on the first version of ALPODS. However. the current version of Cinderella uses the new ALPODS XAI that generated ALPODSclassifierV2.R

Hence, the only purpose of this data is to serve as an example for the functionality of the package.
}
\source{
ALPODSclassifier.R
}

\examples{
data(PopsMat)
str(PopsMat)
}
\keyword{datasets}
