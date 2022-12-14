\name{Cinderella-package}
\alias{Cinderella-package}
\alias{Cinderella}
\docType{package}
\title{
\packageTitle{Cinderella}
}
\description{
\packageDescription{Cinderella}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{Cinderella}

Package is based on [Hoffman et al., 2022].

\packageIndices{Cinderella}

\tabular{ll}{
Package: \tab Cinderella\cr
Type: \tab Package\cr
Version: \tab 0.0.7\cr
Date: \tab 2022-04-01\cr
License: \tab CC BY-NC-SA 4.0\cr
}

}
\author{
\packageAuthor{Cinderella}

Maintainer: \packageMaintainer{Cinderella}
}
\references{
[Hoffman et al., 2022] Hoffman, J., Thrun, M. C., Röhnert, M., Von Bonin, M., Oelschlägel, U., Neubauer, A., Ultsch, A., Brendel, C.: Identification of critical hemodilution by artificial intelligence in bone marrow assessed for minimal residual disease analysis in acute myeloid leukemia: the Cinderella method, Cytometry: Part A, in revision, 2022.
}

\keyword{Cinderella}

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

TransV=PrepropCompensatedData(DataV,cl = cl)
names(TransV)

#Classifier was generated by ALPODS. see ALPODSclassifier.RD for details
Funame="ALPODSclassifierV2"
PopsMatV=GeneratePopulations(TransV,Funame,cl = cl) 
parallel::stopCluster(cl)
PopsMatVerification=round(PopsMatV$PopsMat*100,3)

#prediction of new samples of h1 panel based on a given trainingset 
#for trainingset priorly the functions ReadAll_LMD_files, 
# PrepropCompensatedData and GeneratePopulations
#wer applied, Target_Marburg stores the the holdrinet values
CinderellaResult=FuzzyEnsemble(PopMat_Marburg,Target_Marburg,

PopsMatVerification,Operator = min)
}
}
