#' STATISTICAL EFFECTS
#'
#' @description This function calculates various statistical measures for combined variables, in particular some measures
#' such as rati odds, risk ratio or the difference in the proportions of two groups in relation
#' to the binarius compost event.
#'
#'
#' @param p0_e1 numeric parameter, probability of occurrence E1 by the control group
#' @param p0_e2 numeric parameter, probability of occurrence E2 by the control group
#' @param p1_e1 numeric parameter, probability of occurrence E1 by the intervention group
#' @param p1_e2 numeric parameter, probability of occurrence E2 by the intervention group
#' @param rho numeric parameter, Pearson correlation between E1 i E2
#' @param effect_ce character, specifies the type of statistical measure that is calculated
#'
#' @export
#'
#' @return Returns the desired effect of the composite binary event and the effects of the events
#' @details The input parameters representing the probability of the events
#' taking place are limited between 0 and 1, without including both values. Pearson's correlation
#' must be within the confidence interval that allows the combined variable according to the probabilities given.
#' To calculate this confidence interval you can use lower_corr and upper_corr functions that you can find in this package.
#' For defect, if you don't specify the type of effect you want to obtain,
#' it calculates the difference in proportions.
#' 
effect_ce <- function(p0_e1, p0_e2, p1_e1, p1_e2, rho, effect_ce = "diff"){
  if(p0_e1 < 0 || p0_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p0_e2 < 0 || p0_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(p1_e1 < 0 || p1_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p1_e2 < 0 || p1_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(rho <= max(c(lower_corr(p0_e1,p0_e2),lower_corr(p1_e1,p1_e2)))  ||  rho >= max(c(upper_corr(p0_e1,p0_e2),upper_corr(p1_e1,p1_e2)))){
    stop("The correlations of events must be in the correct interval")
  }else if(effect_ce != "rr" && effect_ce != "diff" && effect_ce != "or"){
    stop("You have to choose between odds ratio, relative risk or difference in proportions")
  }

    if(effect_ce == "diff"){
      diff_e1 = p1_e1 - p0_e1
      diff_e2 = p1_e2 - p0_e2
      effect = prob_ce(p1_e1,p1_e2,rho) - prob_ce(p0_e1,p0_e2,rho)
      effect_out <- data.frame(diff_e1,diff_e2,effect)
    }else if(effect_ce == "rr"){
      rr_e1 = p1_e1 / p0_e1
      rr_e2 = p1_e2 / p0_e2
      effect = prob_ce(p1_e1,p1_e2,rho)/prob_ce(p0_e1,p0_e2,rho)
      effect_out = data.frame(rr_e1,rr_e2,effect)
    }else if(effect_ce == "or"){
      O10= p0_e1/(1-p0_e1)
      O20= p0_e2/(1-p0_e2)
      or_e1 = (p1_e1/(1-p1_e1))/(p0_e1/(1-p0_e1))
      or_e2 = (p1_e2/(1-p1_e2))/(p0_e2/(1-p0_e2))
      effect = ((O10*or_e1+1)*(O20*or_e2+1)-1-rho*sqrt(or_e1*or_e2*O10*O20))*(1+rho*sqrt(O10*O20))/
        (((1+O10)*(1+O20)-1-rho*sqrt(O10*O20))*(1+rho*sqrt(or_e1*or_e2*O10*O20)))
      effect_out = data.frame(or_e1,or_e1,effect)
    }
    colnames(effect_out) <- c("Effect E1","Effect E2","Effect CE")
    return(effect_out)
}
