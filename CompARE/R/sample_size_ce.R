#' Sample size for composite binary endpoints
#'
#' @description This function calculates the value of the sample size according to its input parameters.
#'
#' @param p0_e1 numeric parameter, probability of occurrence E1 by the control group
#' @param p0_e2 numeric parameter, probability of occurrence E2 by the control group
#' @param type_e1 Effect of the event E1
#' @param eff_e1 numeric parameter, effect of the event E1
#' @param type_e2 Effect of the event E2
#' @param eff_e2 numeric parameter, effect of the event E2
#' @param effect_ce Effect to measure the sample size
#' @param rho numeric parameter, correlation of pearson between two events E1 and E2
#' @param alpha level of confidencea alpha
#' @param beta level of confidence beta
#' @param unpooled Variance class used in the calculation of the sample size
#'
#' @export
#'
#' @return Return the sample size for composite binary endpoints based on the anticipated values of the composite components
#' and the association between them in terms of Pearson's correlation.
#' @details The entry parameters representing the probability of the events taking place are limited between 0 and 1,
#'  without including both values. The values of the primary events should be calculated giving a higher probability of observation to the control group.
#'  Pearson's correlation must be within the confidence interval allowed
#'  by the combined variable according to the probabilities given. To calculate this confidence interval you can use lower_corr and upper_corr functions
#'  that you can find in this package. By default, if the type of effect
#'  with which the resulting large sample is to be associated is not specified,
#'  the difference in proportions is used. When specifying the levels of alpha and beta meaning,
#'  they may not be higher than 1, as well as the following type of variance that is desired to be used
#'  in the calculation of the sample size.
sample_size_ce <- function(p0_e1,p0_e2,type_e1,eff_e1,type_e2,eff_e2,effect_ce  = "diff",rho, alpha = 0.05, beta = 0.2, unpooled = "unpooled Variance"){

  if(p0_e1 < 0 || p0_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p0_e2 < 0 || p0_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(type_e1 != "diff" && type_e1 != "rr" && type_e1 != "or"){
    stop("You have to choose between odds ratio, relative risk or difference in proportions")
  }else if((type_e1 == "diff" && eff_e1 > 0) || (type_e1 == "or" && (eff_e1 < 0 || eff_e1 > 1)) || (type_e1 == "rr" && (eff_e1 < 0 || eff_e1 > 1))){
    stop("The effect of the event E1 is not right")
  }else if(type_e2 != "diff" && type_e2 != "rr" && type_e2 != "or"){
    stop("You have to choose between odds ratio, relative risk or difference in proportions")
  }else if((type_e2 == "diff" && eff_e2 > 0) || (type_e2 == "or" && (eff_e2 < 0 || eff_e2 > 1)) || (type_e2 == "rr" && (eff_e2 < 0 || eff_e2 > 1))){
    stop("The effect of the event E2 is not right")
  }else if(effect_ce != "diff" && effect_ce != "rr" && effect_ce != "or"){
    stop("You have to choose between odds ratio, relative risk or difference in proportions")
  }else if(rho <= lower_corr(p0_e1,p0_e2)  ||  rho >= upper_corr(p0_e1,p0_e2)){
    stop("The correlations of events must be in the correct interval")
  }else if( 0 > alpha || alpha > 1){
    stop("Alpha value must be number between 0 and 1")
  }else if( 0 > beta || beta > 1){
    stop("Beta value must be number between 0 and 1")
  }else if(unpooled != "unpooled Variance" && unpooled != "Pooled Variance"){
    stop("You must choose between pooled and unpooled variance")
  }


  if(type_e1 == "or"){
    p1_e1= (eff_e2*p0_e1/(1-p0_e1))/(1+(eff_e2*p0_e1/(1-p0_e1)))
  }else if(type_e1 == "rr"){
    p1_e1 = eff_e1 * p0_e1
  }else if(type_e1 == "diff"){
    p1_e1 = eff_e1 + p0_e1
  }

  if(type_e2 == "or"){
    p1_e2 = (eff_e2*p0_e2/(1-p0_e2))/(1+(eff_e2*p0_e2/(1-p0_e2)))
  }else if(type_e2 == "rr"){
    p1_e2 = eff_e2 * p0_e2
  }else if(type_e2 == "diff"){
    p1_e2 = eff_e2 + p0_e2
  }

  p0_CBE = 1- (1-p0_e1)*(1-p0_e2)*( 1+ rho*sqrt(p0_e1*p0_e2/((1-p0_e1)*(1-p0_e2)) ))
  p1_CBE = 1- (1-p1_e1)*(1-p1_e2)*( 1+ rho*sqrt(p1_e1*p1_e2/((1-p1_e1)*(1-p1_e2)) ))


  if(effect_ce == "rr"){
    rr_CBE <- effect_ce(p0_e1, p0_e2, p1_e1, p1_e2, rho, type = "rr")[1,3]
    if(unpooled =="unpooled Variance"){
      samp = 2*(((qnorm(1-alpha,0,1)+qnorm(1-beta,0,1))/(log(rr_CBE)))^2*( (1-rr_CBE*p0_CBE)/(rr_CBE*p0_CBE) + (1-p0_CBE)/p0_CBE))
    }else if(unpooled =="Variance"){
      p = (rr_CBE*p0_CBE + p0_CBE)/2
      # sample size per group
      samp = 2*(( (qnorm(1-alpha,0,1)* sqrt(2*(1-p)/p) + qnorm(1-beta,0,1)* sqrt( (1-rr_CBE*p0_CBE)/(rr_CBE*p0_CBE) + (1-p0_CBE)/p0_CBE) )/(log(rr_CBE)) )^2  )
    }
  }else if(effect_ce == "or"){
    or_CBE = effect_ce(p0_e1, p0_e2, p1_e1, p1_e2, rho, type = "or")[1,3]
    p1 = (or_CBE*p0_CBE/(1-p0_CBE))/(1+(or_CBE*p0_CBE/(1-p0_CBE)))
    if(unpooled=="unpooled Variance"){
      samp = ((qnorm(1-alpha,0,1)+qnorm(1-beta,0,1))/(log(or_CBE)))^2*( 1/(p0_CBE*(1-p0_CBE)) + 1/(p1*(1-p1)) )
    }else{
      samp = ((qnorm(1-alpha,0,1)* sqrt(2/(((p1 + p0_CBE)/2)*(1-((p1 + p0_CBE)/2)))) + qnorm(1-beta,0,1)* sqrt(1/(p0_CBE*(1-p0_CBE)) + 1/(p1*(1-p1))))/(log(or_CBE)))^2
    }
  }else{
    diff_CBE <- effect_ce(p0_e1, p0_e2, p1_e1, p1_e2, rho, type = "rr")[1,3]
    if(unpooled=="unpooled Variance"){
      samp = ((qnorm(1-alpha,0,1) +qnorm(1-beta,0,1))/diff_CBE)^2*( p0_CBE*(1-p0_CBE) + (diff_CBE+p0_CBE)*(1-p0_CBE-diff_CBE))
    }else if(unpooled=="Variance"){
      p = (diff_CBE + 2 * p0_CBE)/2
      # sample size per group
      samp = ((qnorm(1-alpha,0,1)* sqrt(2*p*(1-p)) +  qnorm(1-beta,0,1)* sqrt( p0_CBE*(1-p0_CBE) + (diff_CBE+p0_CBE)*(1-p0_CBE-diff_CBE)))/diff.CBE)^2
    }

  }
  return(samp)

}
