# 23.12.2009 version 1, mvdl
qqNormalLimit <- function(y, p , iLambda, alpha)
{

   par  <- fitNormal(y[iLambda], p[iLambda])
   yHat <- qnorm(p, par$mu, par$sigma)
   res  <- y - yHat
   sigmaE <- sqrt(mean(res[iLambda]^2))
   
   L <- getLplusLmin(sigmaE, alpha)

   return(list(limit=c(Left=L$Lmin,Right=L$Lplus),
               residuals=res,
               sigmaE=sigmaE,
               k=par$k,
               lambda=par$lambda,
               R2=par$R2))
}
