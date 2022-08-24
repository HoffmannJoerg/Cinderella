# Cinderella



## Table of contents

1. [Description](#description)
2. [Installation](#installation)
3. [Use case](#use-cases)
4. [Additional information](#additional-information)
5. [References](#references)

## Description

Cinderella quantifes of peripheral blood contamination in bone marrow samples of the H1 panel, see [1] for details.

## Installation

#### Installation using CRAN
Install automatically with all dependencies via

```R
install.packages("Cinderella",dependencies = T)

# automatic installation of all relevant packages:
Suggested=c("parallel", "caret", "plyr", "DataVisualizations", "stringr", "flowCore", "brnn", "gbm", "earth", "elasticnet","stringi")

for(i in 1:length(Suggested)) {
  if (!requireNamespace(Suggested[i], quietly = TRUE)) {
    message(paste("Installing the package", Suggested[i]))
    install.packages(Suggested[i], dependencies = T)
  }
}
```

## Use Case

### Quantifeication of peripheral blood contamination in bone marrow samples
The package Cinderella uses specific information of *.lmd files that can be read in with the ReadAll_LMD_files function.
In general, raw data of the H1 Panel is expected. The data is compensated within this package using information stored in the *.lmd files and then normalized. The populations are then defined by the ALPODS XAI [2]. Prediction of peripheral blood contamination in bone marrow samples is performed as described in [1] with data accessible in [3]

```R
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

#Classifier was generated by ALPODS. see [2] for details
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

```

## Additional information

 License: CC BY-NC-SA 4.0
 
 Dependencies: R (>= 3.5.0)          


## References
1. Hoffman, J., Thrun, M. C., Röhnert, M., Von Bonin, M., Oelschlägel, U., Neubauer, A., Ultsch, A., Brendel, C.: Identification of critical hemodilution by artificial intelligence in bone marrow assessed for minimal residual disease analysis in acute myeloid leukemia: the Cinderella method, Cytometry: Part A, in 2nd revision, 2022.
2. Ultsch, A., Hoffman, J., Röhnert, M., Von Bonin, M., Oelschlägel, U., Brendel, C., & Thrun, M. C.: An explainable AI system for the diagnosis of high-dimensional biomedical data, in revision, 2022.
3. Thrun, M. C., Hoffman, J., Röhnert, M., Von Bonin, M., Oelschlägel, U., Brendel, C., & Ultsch, A.: Flow Cytometry datasets consisting of peripheral blood and bone marrow samples for the evaluation of explainable artificial intelligence methods, Data in Brief, Vol. 43, pp. 108382, DOI: 10.1016/j.dib.2022.108382, 2022. 
