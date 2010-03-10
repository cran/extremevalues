# 23.12.2009 version 1, mvdl
qqParetoLimit <- function(y, p , iLambda, alpha)
{

   par  <- fitPareto(y[iLambda], p[iLambda])
   yHat <- qpareto(p, par$ym, par$alpha)
   res  <- log(y) - log(yHat)
   sigmaE <- sqrt(mean(res[iLambda]^2))
   
   L <- getLplusLmin(sigmaE, alpha)

   return(list(limit=c(Left=L$Lmin,Right=L$Lplus),
               residuals=res,
               sigmaE=sigmaE,
               k=par$k,
               lambda=par$lambda,
               R2=par$R2))
}
