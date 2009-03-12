# file  : getExpLimit.r
# author: Mark van der Loo (mark.vanderloo@gmail.com)
# 
# Determine outlier limit based exponential distribution.
#
# INPUT
# y     : vector of observed values between pmin and pmax
# p     : vector of observed quantiles (y_i estimates the p_i'th quantile)
# N     : total number of observations
# rho   : outlier parameter
#
# OUTPUT (list)
# lambda: estimate parameter
# R2    : R-squared value of fit.
#
# History
# 22.10.2009    version 1
#


getExponentialLimit <- function(y, p, N, rho)
{
   param <- fitExponential(y, p)
   ell <- log(N/rho)/param$lambda
   return(list(lambda=param$lambda, 
               R2=param$R2,
               nFit=length(y),
               limit=ell)
         )
}
