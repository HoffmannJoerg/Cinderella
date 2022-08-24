\name{RegressionQualityMeasures}
\alias{RegressionQualityMeasures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regression Quality Measures (QMs)
}
\description{
Computes standard Regression Quality Measures (QMs)
}
\usage{
RegressionQualityMeasures(Target, Predicted,

NoFeaturesData, PlotIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Target}{
numerical vector [1:n] of the target
}
  \item{Predicted}{
numerical vector [1:n] of the prediction based on a regression with either training or test data
}
  \item{NoFeaturesData}{
scalar, adjusted R2 requires the dimensionality of the test data resp. training data
}
  \item{PlotIt}{
TRUE: Bland Altman plot is visualized
}
}
\details{

if \code{NoFeaturesData} not given the worst case scenario of \code{NoFeaturesData=n-2} is assumed. For the number of features that are higher, i.e., if \code{NoFeaturesData>n-2}, then this case is statistically not defined. Hence, in that case the upper boundary is always set tp \code{NoFeaturesData=n-2} with a warning.
}
\value{
than named vector of Quality measures called SSE, spearman cor, RMSE, R2(coefficient of determination), adjustedR2 with PredictedTarget
}

\author{
Michael Thrun
}

\seealso{
\code{\link{MLregressor}}
}
\examples{
##ToDo
}
