\name{getLimit}
\keyword{internal}
\alias{getLognormalLimit}
\alias{getExponentialLimit}
\alias{getParetoLimit}
\alias{getWeibullLimit}
\alias{getNormalLimit}
\title{Determine outlier limit}
\description{
Determine outlier limit. These functions are called by the
wrapperfunction getOutlierLimit
}
\usage{
getExponentialLimit(y, p, N, rho)
getLognormalLimit(y, p, N, rho)
getParetoLimit(y, p, N, rho)
getWeibullLimit(y, p, N, rho)
getNormalLimit(y, p, N, rho)
}

\arguments{
\item{y}{Vector of one-dimensional nonnegative data}
\item{p}{Corresponding quantile values}
\item{N}{Number of observations}
\item{rho}{Limiting expexted value}
}
\value{
\item{limit}{The y-value above which less then rho observations are expected}
\item{R2}{R-squared value for the fit}
\item{nFit}{Number of values used in fit (length(y))}
\item{lamda}{(exponential only) Estimated location (and spread) parameter for \eqn{f(y)=\lambda\exp(-\lambda  y)}}
\item{mu}{(lognormal only) Estimated \eqn{{\sf E}(\ln(y))} for lognormal distribution}
\item{sigma}{(lognormal only) Estimated Var(ln(y)) for lognormal distribution}
\item{ym}{(pareto only) Estimated location parameter (mode) for pareto distribution} 
\item{alpha}{(pareto only) Estimated spread parameter for pareto distribution}
\item{k}{(weibull only) estimated power parameter \eqn{k} for weibull distribution}
\item{lambda}{(weibull only) estimated scaling parameter \eqn{\lambda} for weibull distribution}
   }

\details{
The functions fit a model cdf to the observed y and p and returns the
y-value above which less than rho values are expected, given N observations.
See getOutlierLimit for a complete explanation. 

The function returns a list with the following entries:
}

\references{An outlier detection method for economic data, M.P.J. van der
Loo, Submitted to The Journal of Official Statistics (November 2009) }
\author{Mark van der Loo, see www.markvanderloo.eu}
\examples{
y <- 10^rnorm(50);
p <- seq(1,50)/50;
L <- getExponentialLimit(y[10:48],p[10:48],50,0.5);
}

