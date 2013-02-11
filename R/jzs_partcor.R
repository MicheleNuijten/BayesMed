jzs_partcor <-
function(V1,V2,control){
  
  V1 <- (V1-mean(V1))/sd(V1)
  V2 <- (V2-mean(V2))/sd(V2)
  control <- (control-mean(control))/sd(control)
  
  r0 <- sqrt(summary(lm(V1~control))$r.squared)
  r1 <- sqrt(summary(lm(V1~control+V2))$r.squared)
  p0 <- 1
  p1 <- 2
  n  <- length(V1)
  
  jzs_partcorbf <- function(r0,r1,p0,p1,n){
    int <- function(r,n,p,g){
      a <- .5*((n-1-p)*log(1+g)-(n-1)*log(1+g*(1-r^2)))
      exp(a)*dinvgamma(g,shape=.5,scale=n/2)
    }
    bf10 <- integrate(int,lower=0,upper=Inf,r=r1,p=p1,n=n)$value/
      integrate(int,lower=0,upper=Inf,r=r0,p=p0,n=n)$value
    return(bf10)
  }
  
  bf10 <- jzs_partcorbf(r0,r1,p0,p1,n)
  postprob <- bf10/(bf10+1)
  
  return(list(BayesFactor=bf10,
              PosteriorProbability=postprob))
  
}
