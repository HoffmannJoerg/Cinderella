\name{SplitPercentage}
\alias{SplitPercentage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SplitPercentage
}
\description{
Splits data into training and test set by percentage
}
\usage{
SplitPercentage(Data, Target, Percentage)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:d,1:n]        data array of d cases with n variables
}
  \item{Target}{
[1:n]        numerical vector definined the target
}
  \item{Percentage}{
scalar value, percentage of split 
}
}
\details{
number of cases n equals m+k, function can be used for crossvalidation
}
\value{
LIST of
\item{Train}{[1:d,1:m]        numerical matrix of d cases with n variables, the trainingset}
\item{Test}{[1:d,1:k]         numerical matrix of d cases with n variables, the testset}

\item{TargetTrain }{[1:m]        numerical vector definined the target of the training set}
\item{TargetTest }{[1:k]       numerical vector definined the target of the test set}
\item{indtrain }{[1:m] numerical vector of index for the traininset cases}
\item{indtest }{1:k]  numerical vector of index for the test set cases}

}

\author{
Michael Thrun
}



\examples{
#random target
data("PopsMat")
Target=runif(nrow(PopsMat))
SplitPercentage(PopsMat, Target, 80)
}

