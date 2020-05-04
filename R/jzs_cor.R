jzs_cor <-
  function(V1, V2, 
           alternative = c("two.sided","less","greater"),
           n.iter = 10000, n.burnin = 500, standardize = TRUE, subdivisions = 200){
    
    runif(1) # defines .Random.seed
    
    # standardize variables
    if(standardize == TRUE){
      X <- (V1-mean(V1))/sd(V1)
      Y <- (V2-mean(V2))/sd(V2)
    }else {
      X <- V1
      Y <- V2
    }
    
    r <- cor(X,Y)
    n <- length(X)
    
    BF <- jzs_corbf(r,n)
    
    ### the next part is needed to impose an order restriction
    ### for the order restrictions we need to estimate the posterior samples
    
    #==========================================================
    # load JAGS models
    #==========================================================
    
    jags.model.file1 <- system.file("jags", "jags-model-correlation.txt", 
                                    package = "BayesMed")
    
    #========================================================================
    # Estimate Posterior Distribution for the Correlation Coefficient Alpha
    #========================================================================
    
    x <- X
    y <- Y
    
    invSigma <- solve(t(x)%*%x)
    
    jags.data   <- list("n", "x", "y", "invSigma")
    jags.params <- c("alpha", "g")
    jags.inits  <-  list(
      list(alpha = 0.0), #chain 1 starting value
      list(alpha = -0.3), #chain 2 starting value
      list(alpha = 0.3)) #chain 3 starting value
    
    jagssamples <- R2jags::jags(data = jags.data, inits=jags.inits, jags.params, 
                                n.chains = 3, n.iter = n.iter, DIC = TRUE,
                                n.burnin = n.burnin, n.thin = 1, 
                                model.file = jags.model.file1)
    
    # estimate the posterior regression coefficient and scaling factor g
    alpha <- jagssamples$BUGSoutput$sims.list$alpha[,1]
    g  <- jagssamples$BUGSoutput$sims.list$g
    
    #-------------------------------------------------------
    
    # one-sided test?
    
    # save BF for one-tailed test
    # BF21 = 2*{proportion posterior samples of alpha < 0}
    
    propposterior_less <- sum(alpha<0)/length(alpha)
    propposterior_greater <- sum(alpha>0)/length(alpha)
    
    # posterior proportion cannot be zero, because this renders a BF of zero
    # none of the samples of the parameter follow the restriction
    # ergo: the posterior proportion is smaller than 1/length(parameter)
    
    if(propposterior_less==0){
      propposterior_less <- 1/length(alpha)
    }
    
    if(propposterior_greater==0){
      propposterior_greater <- 1/length(alpha)
    }
    
    BF21_less <- 2*propposterior_less
    BF21_greater <- 2*propposterior_greater
    
    
    if(alternative[1]=="less"){
      # BF10 = p(D|a~cauchy(0,1))/p(D|a=0)
      BF10 <- BF
      
      # BF21 = p(D|a~cauchy-(0,1))/p(D|a~cauchy(0,1))
      # BF21 = 2*{proportion posterior samples of alpha < 0}
      BF21 <- BF21_less
      
      BF <- BF10*BF21
      
    } else if(alternative[1]=="greater"){
      # BF10 = p(D|a~cauchy(0,1))/p(D|a=0)
      BF10 <- BF
      
      # BF21 = p(D|a~cauchy+(0,1))/p(D|a~cauchy(0,1))
      # BF21 = 2*{proportion posterior samples of alpha > 0}
      BF21 <- BF21_greater
      
      BF <- BF10*BF21
      
    }
    
    #--------------------------------------------------------
    
    # convert BFs to posterior probability
    # prob cannot be exactly 1 or 0
    prob_r <- BF/(BF+1)
    
    if(prob_r == 1){
      prob_r <- prob_r - .Machine$double.eps
    }
    if(prob_r == 0){
      prob_r <- prob_r + .Machine$double.eps
    }
    
    #==================================================
    
    # convert posterior samples for the regression coefficient x-y to correlation
    cor_coef <- alpha*(sd(x)/sd(y))
    
    #===================================================
    
    res <- list(Correlation=mean(cor_coef),
                BayesFactor=BF,
                PosteriorProbability=prob_r,
                cor_coef_samples=cor_coef,
                jagssamples=jagssamples)
    
    class(res) <- c("jzs_med","list")
    class(res$jagssamples) <- "rjags"
    class(res$cor_coef_samples) <- "CI"
    
    return(res)
  }
