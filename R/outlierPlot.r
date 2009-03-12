# file   : outlierPlot.r
# author : Mark van der Loo
# date   : 19.11.2009
#
# create overview plot of outlier detection
# L is a list resulting from L <- getOutlierLimit(y)
# 
# version 
# 
# 03.12.2009      added normal distribution

outlierPlot <- function(y, L)
{
 phat <- ((1:length(y))-0.5)/length(y);

 p <- 10^seq(-5.00001,-0.00001,0.01);
 p <- unique(sort(c(p,head(phat,length(y)-1))))

 ndig <- max(0,4-floor(log10(abs(L$limit))))

 plot(phat,sort(y),
   main=paste("Outlier plot, rho=",L$rho, "limit=",round(L$limit,ndig),
              "\n using", L$method,"distribution"),
   xlab="p",
   ylab="value")

 rect(L$pmin,-max(y), L$pmax, 1.1*max(y), density=30, col="green")
 points(phat, sort(y))
 yPred <- switch(L$method,
            lognormal = qlnorm(p,L$mu,L$sigma),
            pareto    = qpareto(p,L$ym,L$alpha),
            exponential = (-1/L$lambda)*log(1-p),
            weibull = qweibull(p, L$k, L$lambda),
            normal = qnorm(p,L$mu,L$sigma)
         )
 if ( max(yPred) < max(y) ){
   p <- c(p,1)
   yPred <- c(yPred, max(y)*1.2)
   }
 lines(p,yPred)
 lines(c(0,1),c(L$limit,L$limit))
 abline(L$limit,0,col="red")

 points(tail(phat,L$nOut),sort(y[L$iOut]),pch=16,col="red")

}


