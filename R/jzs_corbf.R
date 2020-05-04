# main function to analytically calculate the BF for correlation
# see Wetzels, R. & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis 
# test for correlations and partial correlations. Psychonomic Bulletin & 
# Review, 19, 1057-1064. The jzs_corbf function is based on updated R code 
# that can handle larger values of n

jzs_corbf <- function(r,n){
  
  bf10 <- try(sqrt((n/2))/gamma(1/2) * 
                integrate(int_cor, lower = 0, upper = Inf, 
                          r = r, n = n, 
                          subdivisions = subdivisions)$value)
  
  # if the evidence is overwhelming, the BF will become infinite
  # to avoid this resulting in an error, give back the max possible number
  if(class(bf10) == "try-error" & grepl("non-finite function value", bf10[1])){
    bf10 <- .Machine$double.xmax
    message("Note: the Error above was caused because the BF from the function 
            jzs_corbf was approaching infinity. To avoid the function from 
            crashing, the returned BF is the largest possible number in R.")
  } else {
    if(class(bf10)=="try-error" & !grepl("non-finite function value", bf10[1])){
      bf10 <- NA
      message("An error occurred. The BF could not be calculated")
    }
  }
  return(bf10)	
}


int_cor <- function(r,n,g){
  exp(((n-2)/2)*log(1+g)+
      (-(n-1)/2)*log(1+(1-r^2)*g)+
      (-3/2)*log(g)+
      -n/(2*g))
}