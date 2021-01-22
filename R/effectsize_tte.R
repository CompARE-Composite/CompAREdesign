effectsize_tte <- function(p0_e1, p0_e2, HR_e1, HR_e2, beta_e1=1, beta_e2=1, case, copula = 'Frank', rho=0.3, rho_type='Spearman'){
 
  # t values
  t <- c(0.0001,seq(0.001,1,0.001))
  
  
  # Copula
  copula0 <- CopulaSelection(copula,rho,rho_type)
  which.copula <- copula0[[1]]
  theta <- copula0[[2]]   
  
  # Marginal distribution and parameters
  MarginSelec <- MarginalsSelection(beta_e1,beta_e2,HR_e1,HR_e2,p0_e1,p0_e2,case,theta,copula=copula)
  T1dist   <- MarginSelec[[1]]
  T2dist   <- MarginSelec[[2]]
  T1pdist  <- MarginSelec[[3]]
  T2pdist  <- MarginSelec[[4]]
  T10param <- MarginSelec[[5]]
  T20param <- MarginSelec[[6]]
  T11param <- MarginSelec[[7]]
  T21param <- MarginSelec[[8]]
  
  # Scale parameters
  b10 <- T10param[[2]]
  b20 <- T20param[[2]]
  b11 <- T11param[[2]]
  b21 <- T21param[[2]]
  
  # Bivariate distribution in control and treatment groups
  distribution0 <- mvdc(copula = which.copula, margins = c(T1dist, T2dist), paramMargins = list(T10param, T20param))
  distribution1 <- mvdc(copula = which.copula, margins = c(T1dist, T2dist), paramMargins = list(T11param, T21param))
  
  
  ##-- Densities for both endpoints
  fT10 <- (beta_e1/b10) * ((t/b10)^(beta_e1-1)) * (exp(-(t/b10)^beta_e1))
  fT11 <- (beta_e1/b11) * ((t/b11)^(beta_e1-1)) * (exp(-(t/b11)^beta_e1))
  fT20 <- (beta_e2/b20) * ((t/b20)^(beta_e2-1)) * (exp(-(t/b20)^beta_e2))
  fT21 <- (beta_e2/b21) * ((t/b21)^(beta_e2-1)) * (exp(-(t/b21)^beta_e2))
  
  ##-- Survival for both endpoints
  ST10 <- exp(-(t/b10)^beta_e1)
  ST11 <- exp(-(t/b11)^beta_e1)
  ST20 <- exp(-(t/b20)^beta_e2)
  ST21 <- exp(-(t/b21)^beta_e2)
  
  ##-- Survival for the composite endpoint
  if(copula=='Frank'){
    Sstar0 <- (-log(1+(exp(-theta*ST10)-1)*(exp(-theta*ST20)-1)/(exp(-theta)-1))/theta)
    Sstar1 <- (-log(1+(exp(-theta*ST11)-1)*(exp(-theta*ST21)-1)/(exp(-theta)-1))/theta)  
  }else if(copula=='Clayton'){
    Sstar0 <- (ST10^(-theta) + ST20^(-theta) - 1)^{-1/theta}
    Sstar1 <- (ST11^(-theta) + ST21^(-theta) - 1)^{-1/theta}
  }else if(copula=='Gumbel'){
    Sstar0 <- exp(-((-log(ST10))^theta + (-log(ST20))^theta)^(1/theta))
    Sstar1 <- exp(-((-log(ST11))^theta + (-log(ST21))^theta)^(1/theta))      
  }
  
  
  ##-- Density, hazards and hazard ratio for the composite
  if(copula=='Frank'){
    fstar0 <- (exp(-theta*ST10)*(exp(-theta*ST20)-1)*fT10 + exp(-theta*ST20)*(exp(-theta*ST10)-1)*fT20)/(exp(-theta*Sstar0)*(exp(-theta)-1))
    fstar1 <- (exp(-theta*ST11)*(exp(-theta*ST21)-1)*fT11 + exp(-theta*ST21)*(exp(-theta*ST11)-1)*fT21)/(exp(-theta*Sstar1)*(exp(-theta)-1))
  }else if(copula=='Clayton'){
    fstar0 <- (ST10^(theta+1) * fT10 + ST20^(theta+1) * fT20)/(Sstar0*(ST10^(-theta) + ST20^(-theta) - 1))
    fstar1 <- (ST11^(theta+1) * fT11 + ST20^(theta+1) * fT21)/(Sstar1*(ST11^(-theta) + ST21^(-theta) - 1))
  }else if(copula=='Gumbel'){
    fstar0 <- Sstar0 * log(Sstar0) * ((-log(ST10))^(theta-1) * fT10 * (-ST10)^(-1) + (-log(ST20))^(theta-1)  * fT20 * (-ST20)^(-1))/((-log(ST10))^theta + (-log(ST20))^theta)
    fstar1 <- Sstar1 * log(Sstar1) * ((-log(ST11))^(theta-1) * fT11 * (-ST11)^(-1) + (-log(ST21))^(theta-1)  * fT21 * (-ST21)^(-1))/((-log(ST11))^theta + (-log(ST21))^theta)
  }
  
  ##-- Hazards and hazard ratio for the composite
  Lstar0 <- (fstar0/Sstar0)
  Lstar1 <- (fstar1/Sstar1)
  HRstar <- (Lstar1/Lstar0)
  
  #-- Summary measures for the HR* (see Schempfer 2009)
  HRstar_int <- rowMeans(cbind(HRstar[-1],rev(rev(HRstar)[-1]))) # Mean of HRs for each interval
  fstar0_int <- rowMeans(cbind(fstar0[-1],rev(rev(fstar0)[-1]))) # Mean of fstar0 for each interval
  fstar1_int <- rowMeans(cbind(fstar1[-1],rev(rev(fstar1)[-1]))) # Mean of fstar1 for each interval
  Lstar0_int <- rowMeans(cbind(Lstar0[-1],rev(rev(Lstar0)[-1]))) # Mean of Lstar0 for each interval
  Lstar1_int <- rowMeans(cbind(Lstar1[-1],rev(rev(Lstar1)[-1]))) # Mean of Lstar1 for each interval
  
  nHR <- (HR_e1+HR_e2)/2                                         # naive HR
  mHR <- mean(HRstar)                                            # Mean of HR
  sAHR_0 <- sum(HRstar_int*fstar0_int)/sum(fstar0_int)           # sHR "_0" indicates that f_0 is used instead of f_1 
  
  # gAHR weighted by f0 and by f0+f1
  gAHR_0 <- exp(sum(log(HRstar_int)*fstar0_int)/sum(fstar0_int)) # gHR "_0" indicates that f_0 is used instead of f_1
  gAHR   <- exp(sum(log(HRstar_int)*(fstar0_int+fstar1_int))/sum(fstar0_int+fstar1_int)) # Alternative gAHR
  
  # AHR weighted by f0 and by f0+f1
  AHR_0_num <- sum(Lstar1_int/(Lstar0_int + Lstar1_int)*fstar0_int)/sum(fstar0_int)
  AHR_0_den <- sum(Lstar0_int/(Lstar0_int + Lstar1_int)*fstar0_int)/sum(fstar0_int)
  AHR_0 <- AHR_0_num/AHR_0_den                                   # AHR "_0" indicates that f_0 is used instead of f_1

  AHR_num <- sum(Lstar1_int/(Lstar0_int + Lstar1_int)*(fstar0_int+fstar1_int))/sum(fstar0_int+fstar1_int)
  AHR_den <- sum(Lstar0_int/(Lstar0_int + Lstar1_int)*(fstar0_int+fstar1_int))/sum(fstar0_int+fstar1_int)
  AHR <- AHR_num/AHR_den                                         # AHR "_0" indicates that f_0 is used instead of f_1
    
  # RMST
  RMST_0 <- integrate(Sstar, dist1=T1pdist,dist2=T2pdist,param1=T10param,param2=T20param,dist_biv= distribution0, lower=0,upper=1,subdivisions=1000)$value
  RMST_1 <- integrate(Sstar, dist1=T1pdist,dist2=T2pdist,param1=T11param,param2=T21param,dist_biv= distribution1, lower=0,upper=1,subdivisions=1000)$value
  
  # Probability of event during follow_up
  pstar_0 <- 1 - Sstar0_func(1,dist1=T1pdist,dist2=T2pdist,param1=T10param,param2=T20param,dist_biv= distribution0)
  pstar_1 <- 1 - Sstar1_func(1,dist1=T1pdist,dist2=T2pdist,param1=T11param,param2=T21param,dist_biv= distribution1)
  
  # Median
  limits <- c(0,10)                                                                         # The first and the last values must be in opposite signs for the function
  Med_0 <- uniroot(Sstar0_func_perc, interval=limits,extendInt="yes", perc=0.5,dist1=T1pdist,dist2=T2pdist,param1=T10param,param2=T20param,dist_biv= distribution0)$root  # Find the root (value which equals the function to zero). extendInt="yes": 'interval limits' is extended automatically if necessary
  Med_1 <- uniroot(Sstar1_func_perc, interval=limits,extendInt="yes", perc=0.5,dist1=T1pdist,dist2=T2pdist,param1=T11param,param2=T21param,dist_biv= distribution1)$root  # Find the root (value which equals the function to zero). extendInt="yes": 'interval limits' is extended automatically if necessary
  
  # Probabilities p11,p21 (Although we do not need to calculate the ARE)
  p11 <- 1-exp(-(1/b11)^beta_e1)
  p21 <- 1-exp(-(1/b21)^beta_e2)
  
  ##-- Simplified versions of the functions
  # selected_times <- t %in% c(0.0001,seq(0.1,1,0.1))  # Only stored 10 points per function
  # simplified_HRstar <- HRstar[selected_times]  
  # simplified_Sstar0 <- Sstar0[selected_times]  
  # simplified_Sstar1 <- Sstar1[selected_times]
  # simplified_Lstar0 <- Lstar0[selected_times]  
  # simplified_Lstar1 <- Lstar1[selected_times]
  
  
  return(list('naive HR' = nHR,
              'mean HR' = mHR,
              'sAHR' = sAHR_0,
              'gAHR' = gAHR,
              'AHR' = AHR,
              'RMST ratio' = RMST_1/RMST_0,
              'Median Ratio' = Med_1/Med_0))
}
