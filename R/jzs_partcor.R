jzs_partcor <-
  function(V1,V2,control,
           alternative=c("two.sided","less","greater"),
           n.iter=10000,n.burnin=500,standardize=TRUE){
    
    runif(1) # defines .Random.seed
    
    # standardize variables
    if(standardize==TRUE){
      M <- (V1-mean(V1))/sd(V1)
      Y <- (V2-mean(V2))/sd(V2)
      X <- (control-mean(control))/sd(control)
    } else {
      M <- V1
      Y <- V2
      X <- control      
    }
    
    r0 <- sqrt(summary(lm(M~X))$r.squared)
    r1 <- sqrt(summary(lm(M~X+Y))$r.squared)
    p0 <- 1
    p1 <- 2
    n  <- length(X)
    
    BF <- jzs_partcorbf(r0,r1,p0,p1,n)
    
    ### the next part is needed to impose order restrictions
    ### for the order restrictions we need to estimate the posterior samples
    
    #==========================================================
    # load JAGS models
    #==========================================================
    
    jags_model_partcor <- system.file("jags", 
                                      "jags-model-partial_correlation.txt", 
                                    package = "BayesMed")
    
    #==========================================================
    # BF FOR PARTIAL CORRELATION (MY|X)
    #==========================================================
    
    x <- cbind(X,M)
    y <- Y
    
    V <- solve(t(x)%*%x) #NB I switched to the notation from Ntzoufras, p. 167
    
    jags.data   <- list("n", "x", "y", "V")
    jags.params <- c("theta")
    jags.inits  <-  list(
      list(theta = c(0.0,0.3)),  #chain 1 starting value
      list(theta = c(0.3, 0.0)), #chain 2 starting value
      list(theta = c(-.15,.15))) #chain 3 starting value
    
    jagssamples <- R2jags::jags(data = jags.data, inits = jags.inits, jags.params, 
                                n.chains = 3, n.iter = n.iter, DIC = TRUE,
                                n.burnin = n.burnin, n.thin = 1, 
                                model.file = jags_model_partcor)
    
    beta <- jagssamples$BUGSoutput$sims.list$theta[ ,2]
    
    #-------------------------------------------------------
    
    # one-sided test?
    
    # save BF for one-tailed test
    # BF21 = 2*{proportion posterior samples of beta < 0}
    propposterior_less <- sum(beta<0)/length(beta)
    propposterior_greater <- sum(beta>0)/length(beta)
    
    # posterior proportion cannot be zero, because this renders a BF of zero
    # none of the samples of the parameter follow the restriction
    # ergo: the posterior proportion is smaller than 1/length(parameter)
    
    if(propposterior_less==0){
      propposterior_less <- 1/length(beta)
    }
    
    
    if(propposterior_greater==0){
      propposterior_greater <- 1/length(beta)
    }
    
    BF21_less <- 2*propposterior_less
    BF21_greater <- 2*propposterior_greater
    
    if(alternative[1]=="less"){
      # BF10 = p(D|b~cauchy(0,1))/p(D|b=0)
      BF10 <- BF
      
      # BF21 = p(D|b~cauchy-(0,1))/p(D|b~cauchy(0,1))
      # BF21 = 2*{proportion posterior samples of beta < 0}
      BF21 <- BF21_less
      
      BF <- BF10*BF21
      
    } else if(alternative[1]=="greater"){
      # BF10 = p(D|b~cauchy(0,1))/p(D|b=0)
      BF10 <- BF
      
      # BF21 = p(D|b~cauchy+(0,1))/p(D|b~cauchy(0,1))
      # BF21 = 2*{proportion posterior samples of beta > 0}
      BF21 <- BF21_greater
      
      BF <- BF10*BF21
      
    }
    
    #---------------------------------------------------
    
    # convert BFs to posterior probability
    # prob cannot be exactly 1 or 0
    prob_b <- BF/(BF+1)
    
    if(prob_b == 1){
      prob_b <- prob_b - .Machine$double.eps
    }
    if(prob_b == 0){
      prob_b <- prob_b + .Machine$double.eps
    }
    
    
    #====================================================
    
    res <- list(PartCoef=mean(beta),
                BayesFactor=BF,
                PosteriorProbability=prob_b,
                beta_samples=beta,
                jagssamples=jagssamples)
    
    class(res) <- c("jzs_med","list")
    class(res$jagssamples) <- "rjags"
    class(res$beta_samples) <- "CI"
    
    return(res)
    
  }
