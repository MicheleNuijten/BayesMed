jzs_med <-
  function(independent,dependent,mediator,...){
    
    # independent = vector with values for independent variable
    # dependent = vector with values for dependent variable
    # mediator = vector with values for mediating variable
    
    # sample size
    n <- length(independent) 
    
    X <- independent
    Y <- dependent
    M <- mediator
    
    
    # calculate BF for a and b
    BFa    <- jzs_cor(X,M,...)$BayesFactor
    BFb    <- jzs_partcor(M,Y,control=X,...)$BayesFactor
    
    # convert BFs to posterior probability
    # prob cannot be exactly 1 or 0
    
    prob_a <- BFa/(BFa+1)
    
    if(prob_a == 1){
      prob_a <- prob_a - .Machine$double.eps
    }
    if(prob_a == 0){
      prob_a <- prob_a + .Machine$double.eps
    }
    
    #-------------
    
    prob_b <- BFb/(BFb+1)
    
    if(prob_b == 1){
      prob_b <- prob_b - .Machine$double.eps
    }
    if(prob_b == 0){
      prob_b <- prob_b + .Machine$double.eps
    }
    
    # calculate evidence for mediation (EM)
    EM <- prob_a*prob_b
    
    # calculate evidence for tau_accent
    BFt_accent <- jzs_partcor(X,Y,control=M)$BayesFactor
    
    prob_t_accent <- BFt_accent/(BFt_accent+1)
    
    if(prob_t_accent == 1){
      prob_t_accent <- prob_t_accent - .Machine$double.eps
    }
    if(prob_t_accent == 0){
      prob_t_accent <- prob_t_accent + .Machine$double.eps
    }
    
    # BFs mediation
    BF.EM <- EM/(1-EM)
    
    #===============================================================
    
    result <- data.frame(BF=c(BFa,BFb,BFt_accent,BF.EM),
                         PostProb=c(prob_a,prob_b,prob_t_accent,EM))
    
    rownames(result) <- c("alpha","beta","tau_prime","Mediation (alpha*beta)")
    
    class(result) <- c("JZSMed","data.frame")
    
    return(result)
  }
