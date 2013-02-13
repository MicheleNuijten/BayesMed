jzs_med <-
function(independent,dependent,mediator){
  
  # independent = vector with values for independent variable
  # dependent = vector with values for dependent variable
  # mediator = vector with values for mediating variable
  
  # sample size
  n <- length(independent) 
  
  X <- independent
  Y <- dependent
  M <- mediator
  
    
  # calculate BF for a and b
  BFa    <- jzs_cor(X,M)$BayesFactor
  BFb    <- jzs_partcor(M,Y,control=X)$BayesFactor
  
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
  
  # FULL OR PARTIAL MEDIATION
  BFt_accent <- jzs_partcor(X,Y,control=M)$BayesFactor
  
  prob_t_accent <- BFt_accent/(BFt_accent+1)
  
  if(prob_t_accent == 1){
    prob_t_accent <- prob_t_accent - .Machine$double.eps
  }
  if(prob_t_accent == 0){
    prob_t_accent <- prob_t_accent + .Machine$double.eps
  }
  
  E.FM <- prob_a*prob_b*(1-prob_t_accent)
  
  # BFs mediation and full mediation
  BF.EM <- EM/(1-EM)
  BF.E.FM <- E.FM/(1-E.FM)
  
  #===============================================================
  
  result <- list(EvidenceMediation = EM,
                 EvidenceFullMediation = E.FM,
                 BF_Mediation = BF.EM,
                 BF_FullMediation = BF.E.FM,
                 prob_alpha = prob_a,
                 prob_beta = prob_b,
                 prob_tau_accent = prob_t_accent)
  
  class(result) <- c("jzs_med","list")
  
  return(result)
}
