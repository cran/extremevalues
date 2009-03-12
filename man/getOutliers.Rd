\name{getOutliers}
\alias{getOutliers}
\alias{extremevalues}
\title{Detect outliers}
\description{
Detects outliers in one dimensional data, based on the assumption
that the bulk of (the right side of) the observed data distribution can 
be adequately described by a model distribution.
}
\usage{
getOutliers(y, rho=0.1, pval=c(0.5,0.9), method="lognormal")
}
\arguments{
\item{y}{Vector of one-dimensional nonnegative data}
\item{rho}{A value \eqn{y_i} is an outlier if it is above the limit where less then
rho observations are expected. Must be >=0.}
\item{pval}{c(pmin,pmax) quantile limits indicating which data should be used
to fit the model distribution. Must obey 0 < pmin < pmax < 1.}
\item{method}{Model distributiun used to estimate the limit. Choose from
"lognormal" (default), "exponential", "pareto", "weibull" or "normal".}
}

\value{
\item{iOut}{Index vector indicating where y > limit}
\item{nOut}{Number of outliers. The largest nOut values of y are outliers}
\item{limit}{Outlier limit. Elements of y larger then or equal to limit are considered outliers}
\item{Npop}{Length of y}
\item{method}{method}
\item{rho}{The rho-value}
\item{pmin}{pval[1]}
\item{pmax}{pval[2]}
\item{Nfit}{Number of values used in the fit}
\item{R2}{R-squared value for the fit}
\item{lambda}{(exponential distribution) Estimated location (and spread) parameter for \eqn{f(y)=\lambda\exp(-\lambda y)}}
\item{mu}{(lognormal distribution) Estimated \eqn{ E(\ln(y))} for lognormal distribution}
\item{sigma}{(lognormal distribution) Estimated \eqn{Var(ln(y))} for lognormal distribution}
\item{ym}{(pareto distribution) Estimated location parameter (mode) for pareto distribution} 
\item{alpha}{(pareto distribution) Estimated spread parameter for pareto distribution}
\item{k}{(weibull distribution) estimated shape parameter \eqn{k} for weibull distribution}
\item{lambda}{(weibull distribution) estimated scale parameter \eqn{\lambda} for weibull distribution}
\item{mu}{(normal distribution) Estimated \eqn{ E(y)} for normal distribution}
\item{sigma}{(normal distribution) Estimated \eqn{Var(y)} for normal distribution}
   }

\details{
The function sorts the values of y and uses (log)linear regression to fit
the values between the pmin and pmax quantile to the cdf
of a model distribution. Given a model cdf \eqn{F}, the outlier limit \eqn{l}
is the value above which less than \eqn{\rho} values are expected, 
conditional on the total number
of observations in \eqn{y}: \eqn{l=F^{-1}(1-\rho/N|\hat{\theta})}. Here,
\eqn{\theta} are the cdf's estimated parameters.

}
\references{ An outlier detection method for economic data, M.P.J. van der
Loo, Submitted to The Journal of Official Statistics (November 2009) 

The file <your R directory>/R-<version>/library/extremevalues/extremevalues.pdf
contains a worked example. It can also be downloaded from my website.
}
\author{Mark van der Loo, see www.markvanderloo.eu}
\examples{
y <- c(10^rnorm(50),500);
L <- getOutliers(y,rho=0.5);
outlierPlot(y,L);
}

