\name{MLregressor}
\alias{MLregressor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MLregressor
}
\description{
General regression method for machine learning based on  package \pkg{caret}.
}
\usage{
MLregressor(TrainData, TargetToTrain, TestData,

TargetToTest,Method="brnn",PrintIt=FALSE,

PlotIt=FALSE,SkipCheck=TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{TrainData}{
[1:d,1:m]        data array of d cases with n variables, the trainingset
}
  \item{TargetToTrain}{
[1:m]        numerical vector definined the target of the training set
}
  \item{TestData}{
[1:d,1:k]        data array of d cases with n variables, the testset
}
  \item{TargetToTest}{
[1:k]        Optional, numerical vector definined the target of the test set
}
  \item{Method}{
Optional, string definined the method for package \pkg{caret}
}
  \item{PrintIt}{
Optional, Prints some output
}
  \item{PlotIt}{
Optional, generates a plot
}
  \item{SkipCheck}{
Optional, TRUE:: skips check of regression method is installed
}
  \item{\dots}{
further arguments used in the method called in  \pkg{caret}
}
}
\details{
\pkg{caret} defines up to over 200 classifiers and regression methods
}
\value{
 a LIST with
\item{PredictedTarget }{Description of 'comp1'}
\item{Model}{Description of 'comp2'}
\item{QMs}{if TargetToTest given than named vector of Quality measures called SSE, spearman cor, RMSE, R2(coefficient of determination), adjustedR2 with PredictedTarget, otherwise NULL }
}

\author{
Michael Thrun
}
\note{
If a package that caret uses is not installed, this function tries to install it automatically
}

\examples{
#random target
data("PopsMat")
Target=runif(nrow(PopsMat))
V=SplitPercentage(PopsMat, Target, 50)
#this wil result in a lot of warnings because number of cases is to low
#and target is random

regression=MLregressor(V$Train,V$TargetTrain,V$Test,V$TargetTest)
regression$PredictedTarget
regression$QMs

}
