# file  : getLognormalLimit.r
# author: Mark van der Loo (mark.vanderloo@gmail.com)
# 
# Determine outlier limit assuming lognormal distribution.
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

getLognormalLimit <- function(y, p, N, rho)
{
   param <- fitLognormal(y,p)
   ell <- exp(sqrt(2)*param$sigma*invErf(1-2*rho/N)+param$mu)
   return(list(mu=param$mu, 
               sigma=param$sigma,
               nFit=length(y),
               R2=param$R2,
               limit=ell)
         )
}
