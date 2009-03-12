\name{outlierPlot}
\alias{outlierPlot}
\title{Plot results of outlierdetection}
\description{
Plot an overview of the results of the outlier detection
performed with the getOutliers function.
}

\arguments{
\item{y}{A vector of values}
\item{L}{The result of L <- getOutliers(y,...)}
}

\details{
The values of y are sorted and plotted against
their estimated p-values. Outliers are plotted
in red. A solid black line indicates
the cumulative density distribution that was fitted
to describe the position of the (p,y) pairs, a red line
indicates the outlier limit, and a green background
indicates which points are used in the fit.
}

\author{Mark van der Loo, www.markvanderloo.eu} 
\references{
The file <your R directory>/R-<version>/library/extremevalues/extremevalues.pdf
contains a worked example. It can also be downloaded from my website.
}

\examples{
y<-10^rnorm(300)
y<-c(y,500);
L <- getOutliers(y,rho=0.5)
outlierPlot(y,L)
}






