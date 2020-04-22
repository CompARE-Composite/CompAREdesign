#' Effect for composite binary endpoints
#'
#' @description This function calculates different effect measures for binary composite outcomes. In particular,
#' it allows for computing the risk difference, risk ratio and odds ratio.
#'
#' @param p0_e1 numeric parameter, probability of occurrence E1 in the control group
#' @param p0_e2 numeric parameter, probability of occurrence E2 in the control group
#' @param p1_e1 numeric parameter, probability of occurrence E1 in the intervention group
#' @param p1_e2 numeric parameter, probability of occurrence E2 in the intervention group
#' @param rho numeric parameter, Pearson correlation between E1 and E2
#' @param effm_ce character, specifies the effect measure to be calculated  for the composite endpoint (effm_ce = "diff" for difference of proportions, effm_ce = "rr" for risk ratio, effm_ce = "or" for odds ratio)
#'
#' @export
#'
#' @return Returns the effect for the composite binary endpoint and the effects for the composite components
#'
#'  @details The input parameters represent the probability of the composite components and Pearson's correlation between the two components.
#' Note that Pearson's correlation takes values between two bounds that depend on the probabilities p0_e1 and p0_e2.
#' To calculate the correlation bounds you can use the R functions lower_corr and upper_corr, available in this package.
#'
#'
#'  @references Bofill Roig, M., & Gómez Melis, G. (2019). A new approach for sizing trials with composite binary endpoints using anticipated marginal values and accounting for the correlation between components. Statistics in Medicine, 38(11), 1935–1956. https://doi.org/10.1002/sim.8092
#'
#'
effect_cbe <- function(p0_e1, p0_e2, p1_e1, p1_e2, rho, effm_ce = "diff"){
  if(p0_e1 < 0 || p0_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p0_e2 < 0 || p0_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(p1_e1 < 0 || p1_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p1_e2 < 0 || p1_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(rho <= max(c(lower_corr(p0_e1,p0_e2),lower_corr(p1_e1,p1_e2)))  ||  rho >= max(c(upper_corr(p0_e1,p0_e2),upper_corr(p1_e1,p1_e2)))){
    stop("The correlation must be in the correct interval")
  }else if(effm_ce != "rr" && effm_ce != "diff" && effm_ce != "or"){
    stop("You have to choose between odds ratio, relative risk or difference in proportions")
  }

    if(effm_ce == "diff"){
      diff_e1 = p1_e1 - p0_e1
      diff_e2 = p1_e2 - p0_e2
      effect = prob_ce(p1_e1,p1_e2,rho) - prob_ce(p0_e1,p0_e2,rho)
      effect_out <- data.frame(diff_e1,diff_e2,effect)
    }else if(effm_ce == "rr"){
      rr_e1 = p1_e1 / p0_e1
      rr_e2 = p1_e2 / p0_e2
      effect = prob_ce(p1_e1,p1_e2,rho)/prob_ce(p0_e1,p0_e2,rho)
      effect_out = data.frame(rr_e1,rr_e2,effect)
    }else if(effm_ce == "or"){
      O10= p0_e1/(1-p0_e1)
      O20= p0_e2/(1-p0_e2)
      or_e1 = (p1_e1/(1-p1_e1))/(p0_e1/(1-p0_e1))
      or_e2 = (p1_e2/(1-p1_e2))/(p0_e2/(1-p0_e2))
      effect = ((O10*or_e1+1)*(O20*or_e2+1)-1-rho*sqrt(or_e1*or_e2*O10*O20))*(1+rho*sqrt(O10*O20))/
        (((1+O10)*(1+O20)-1-rho*sqrt(O10*O20))*(1+rho*sqrt(or_e1*or_e2*O10*O20)))
      effect_out = data.frame(or_e1,or_e2,effect)
    }
    colnames(effect_out) <- c("Effect E1","Effect E2","Effect CE")
    return(effect_out)
}
