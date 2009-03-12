# 19.10.2009, changed p-estimator
# 24.11.2009, added Weibull distribution
getOutliers <- function(y, rho=0.1, pval=c(0.5,0.9), method="lognormal")
{

   if ( !is.vector(y) ) 
      stop("First argument is not of type vector")
   if ( sum(y < 0) > 0 & !(method == "normal") )
      stop("First argument contains nonpositive values")
   if ( ! method %in% c("lognormal", "pareto", "exponential", "weibull", "normal") )
      stop("Invalid distribution (lognormal, pareto, exponential, weibull, normal).")

   Y <- y;
 
   y <- sort(y);
   N <- length(y)
   P <- ((1:N)-0.5)/N
   Lambda <- P >= pval[1] & P<=pval[2]
 
   y <- y[Lambda];
   p <- P[Lambda];
   out <- switch(method,
         lognormal = getLognormalLimit(y, p, N, rho),
         pareto = getParetoLimit(y, p, N, rho),
         exponential = getExponentialLimit(y, p, N, rho),
         weibull = getWeibullLimit(y, p, N, rho),
         normal = getNormalLimit(y, p, N, rho)
         )
   out$iOut = Y > out$limit
   out$nOut = sum(out$iOut)
   out$Npop = length(Y)
   out$rho = rho
   out$pmin = pval[1]
   out$pmax = pval[2]
   out$method=method
   return(out);
}

