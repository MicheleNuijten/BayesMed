plot.jzs_med <-
function(x,...){
  default <- par("mai")
  par(mai=c(0,0,0.4,0))
  
  #plot mediation figure
  plot(1:3,bty="n",type="n",axes=F,xlab="",ylab="",main="",...)
  
  points(2,2.8,pch="M",cex=2)
  points(1.5,1.5,pch="X",cex=2)
  points(2.5,1.5,pch="Y",cex=2)
  
  arrows(1.55,1.6,1.95,2.65,lwd=2,length=.15)
  arrows(2.05,2.65,2.45,1.6,lwd=2,length=.15)
  arrows(1.55,1.5,2.45,1.5,lwd=2,length=.15)
  
  text(1.55,2.2,paste("p(alpha) =",round(x$prob_alpha,2),sep=" "))
  text(2.45,2.2,paste("p(beta) =",round(x$prob_beta,2),sep=" "))
  text(2,1.3,paste("p(tau') =",round(x$prob_tau_accent,2),sep=" "))
  
  par(mai=default)
  
}
