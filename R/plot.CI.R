plot.CI <- function(x,...){
  hist(x,100,freq=FALSE,
       main=expression(paste("posterior distribution for ",alpha*beta,sep=" ")),
       xlab=expression(alpha*beta),...)
  abline(v=quantile(x,c(.025,.975)),lwd=2,col="red")
  legend("topright",
         legend=paste(c("lower bound 95% CI","upper bound 95% CI"),
                      round(quantile(x,c(.025,.975)),2),sep=": "),
         col="red",lty=1,lwd=2)
}