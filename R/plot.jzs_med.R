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
  
  text(1.5,2.2,substitute("p("*{alpha != 0}*"|D) = "*p_a,list(p_a=round(x$prob_alpha,2))))
  text(1.5,2.1,substitute({hat(alpha)==a},list(a=round(mean(x$alpha),2))))
  
  text(2.5,2.2,substitute("p("*beta != 0*"|D) = "*p_b,list(p_b=round(x$prob_beta,2))))
  text(2.5,2.1,substitute({hat(beta)==b},list(b=round(mean(x$beta),2))))
    
  text(2,1.3,substitute("p("*tau*{symbol("\242")} != 0*"|D) = "*p_a,list(p_a=round(x$prob_tau_accent,2))))
  text(2,1.2,substitute(hat(tau*{symbol("\242")}) == t,list(t=round(mean(x$tau_accent),2))))
  
  
  par(mai=default)
  
}