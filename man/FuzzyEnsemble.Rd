\name{FuzzyEnsemble}
\alias{FuzzyEnsemble}

\title{
FuzzyEnsemble
}
\description{
Fuzzy Ensemble of four subsequent methods for regression.
}
\usage{
FuzzyEnsemble(TrainData, TargetToTrain, TestData, TargetToTest, 

Operator = min, CapMin = 0, CapMax = 100, PlotIt = FALSE)
}

\arguments{
  \item{TrainData}{
[1:d,1:m]        numerical matrix of d cases with n variables, the trainingset
}
  \item{TargetToTrain}{
[1:m]        numerical vector definined the target of the training set
}
  \item{TestData}{
[1:d,1:k]         numerical matrix of d cases with n variables, the testset
}
  \item{TargetToTest}{
[1:k]        Optional, numerical vector definined the target of the test set
}
  \item{Operator}{
Optional, combination function to aggregated predictions of subsequent models
}
  \item{CapMin}{
Optional,if regression should be capped to a min
}
  \item{CapMax}{
Optional,if regression should be capped to a max
}
  \item{PlotIt}{
Optional,TRUE: plot
}
}
\details{
This is the Cinderella methods which works on ALPODS generated populations of \code{\link{GeneratePopulations}}
}
\value{
 LIST of
\item{PredictedTarget }{[1:k] numerical vector of Predicted Ensemble Target of the Testset  }
\item{QMs}{Quality measures of SSE, Spearman, RMSE, R2 and adjustedR2 in an named vector based on \code{PredictedTarget} and \code{TargetToTest}}
\item{PredictedTraining}{[1:m] numerical vector of Predicted Ensemble Target of Trainingset  }
\item{Model}{List of three elements with
\itemize{
  \item{\code{Model}}{list of four models}
  \item{\code{SinglePredictionsTest}}{ [1:4,1:k] numerical matrix of four subsequent methods predicted targets}
  
  \item{\code{SinglePredictionsTraining }}{[1:4,1:m]  numerical matrix of four subsequent methods of targets based on training data}
  }
  }
}

\author{
Michael Thrun
}

\seealso{
\code{\link{GeneratePopulations}}
}
\examples{
#random target
data("PopsMat")
Target=runif(nrow(PopsMat))
V=SplitPercentage(PopsMat, Target, 50)
#this wil result in a lot of warnings because number of cases is too low
#and target is random
\donttest{
regression=FuzzyEnsemble(V$Train,V$TargetTrain,V$Test,V$TargetTest)
regression$PredictedTarget
regression$QMs

}
}

\references{
[Hoffman et al., 2022] Hoffman, J., Thrun, M. C., Röhnert, M., Von Bonin, M., Oelschlägel, U., Neubauer, A., Ultsch, A., Brendel, C.: Identification of critical hemodilution by artificial intelligence in bone marrow assessed for minimal residual disease analysis in acute myeloid leukemia: the Cinderella method, Cytometry: Part A, in revision, 2022.
}