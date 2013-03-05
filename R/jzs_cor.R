jzs_cor <-
  function(V1,V2,standardize=TRUE,...){
    
    if(standardize==TRUE){
      V1 <- (V1-mean(V1))/sd(V1)
      V2 <- (V2-mean(V2))/sd(V2)
    }
    
    r <- cor(V1,V2,...)
    n <- length(V1)
    
    jzs_corbf <- function(r,n){
      int <- function(r,n,g){
        a <- .5*((n-2)*log(1+g)-(n-1)*log(1+g*(1-r^2)))
        exp(a)*dinvgamma(g,shape=.5,scale=n/2)
      }
      bf10 <- integrate(int,lower=0,upper=Inf,r=r,n=n)$value
      return(bf10)
    }
    bf10 <- jzs_corbf(r,n)
    postprob <- bf10/(bf10+1)
    
    return(list(Correlation=r,
                BayesFactor=bf10,
                PosteriorProbability=postprob))
  }
