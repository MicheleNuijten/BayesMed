plot.CI <- function(x,...){
  hist(x,100,freq=FALSE,
       main=expression(paste("posterior distribution for ",alpha*beta,sep=" ")),
       xlab=expression(alpha*beta),...)
  abline(v=quantile(x,c(.025,.975)),lwd=2,col="red")
  abline(v=mean(x),lwd=2,col="green")
  abline(v=median(x),lwd=2,col="blue")
  legend("topright",
         legend=c(paste(c("lower bound 95% CI","upper bound 95% CI"),
                      round(quantile(x,c(.025,.975)),3),sep=": "),
                  paste("mean",round(mean(x),3),sep=": "),
                  paste("median",round(median(x),3),sep=": ")),
         col=c("red","red","green","blue"),lty=1,lwd=2)
}