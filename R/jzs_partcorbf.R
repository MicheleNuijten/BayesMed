# main function to analytically calculate the BF for partial correlation
# see Wetzels, R. & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. Psychonomic Bulletin & Review, 19, 1057-1064
# the jzs_partcorbf function is based on updated R code that can handle larger values of n

jzs_partcorbf <- function(r0,r1,p0,p1,n){
  
  
  bf10 <- try(integrate(int_partcor, lower = 0, upper = Inf, r = r1, p = p1, 
                        n = n)$value /
                integrate(int_partcor,lower=0,upper=Inf,r=r0,p=p0,n=n)$value)
  
  # if the evidence is overwhelming, the BF will become infinite
  # to avoid this resulting in an error, give back the max possible number
  if(class(bf10)=="try-error" & grepl("non-finite function value",bf10[1])){
    bf10 <- .Machine$double.xmax
    message("Note: the Error above was caused because the BF from the function 
            jzs_partcorbf was approaching infinity. To avoid the function from 
            crashing, the returned BF is the largest possible number in R.")
  } else {
    if(class(bf10) == "try-error" & !grepl("non-finite function value", 
                                           bf10[1])){
      bf10 <- NA
      message("An error occurred. The BF could not be calculated")
    }
  }
  
  return(bf10)	
}

int_partcor <- function(r,n,p,g){
  exp(
    ((n-1-p)/2)*log(1+g)+
      (-(n-1)/2)*log(1+(1-r^2)*g)+
      (-3/2)*log(g)+
      -n/(2*g))
}