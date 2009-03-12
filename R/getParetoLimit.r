# file  : getParetoLimit.r
# author: Mark van der Loo (mark.vanderloo@gmail.com)
# 
# Determine outlier limit assuming pareto distribution.
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


getParetoLimit <- function(y, p, N, rho)
{
   param <- fitPareto(y,p)
   ell <- param$ym*(N/rho)^{1/param$alpha}
   return(list(ym=param$ym, 
               alpha=param$alpha,
               nFit=length(y),
               R2=param$R2, 
               limit=ell)
         )
}
