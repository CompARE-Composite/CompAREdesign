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
#' @param type character, specifies the type of statistical measure that is calculated
#'
#' @export
#'
#' @return Returns the desired effect of the composite binary event
#' @details  Returns a numeric value. The input parameters representing the probability of the events
#' taking place are limited between 0 and 1, without including both values. Pearson's correlation
#' must be within the confidence interval that allows the combined variable according to the probabilities given.
#' To calculate this confidence interval you can use lower_corr and upper_corr functions that you can find in this package.
#' For defect, if you don't specify the type of effect you want to obtain,
#' it calculates the difference in proportions.
#' @references M.Bofill , G.Gomez , A new approach for sizing trials with composite binary endpoints using anticipated marginal values and accounting for the correlation between components, http://cinna.upc.edu:3838/compare/compareCover/
#' @author Raquel Rovira Salvat
effect_ce <- function(p0_e1, p0_e2, p1_e1, p1_e2, rho, type = "diff"){
  if(p0_e1 < 0 || p0_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p0_e2 < 0 || p0_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(p1_e1 < 0 || p1_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p1_e2 < 0 || p1_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(rho < max(c(lower_corr(p0_e1,p0_e2),lower_corr(p1_e1,p1_e2)))  ||  rho > max(c(upper_corr(p0_e1,p0_e2),upper_corr(p1_e1,p1_e2)))){
    stop("The correlations of events must be in the correct interval")
  }else if(type != "rr" && type != "diff" && type != "or"){
    stop("You have to choose between odds ratio, relative risk or difference in proportions")
  }

    if(type == "diff"){
      effect = prob_ce(p1_e1,p1_e2,rho) - prob_ce(p0_e1,p0_e2,rho)
      diff <- c(out_e1,out_e2,effect)
      effect_ce <- as.data.frame(diff)
    }else if(type == "rr"){
      effect = prob_ce(p1_e1,p1_e2,rho)/prob_ce(p0_e1,p0_e2,rho)
      rr <- c(out_e1,out_e2,effect)
      effect_ce <- as.data.frame(rr)
    }else if(type == "or"){
      O10= p0_e1/(1-p0_e1)
      O20= p0_e2/(1-p0_e2)
      or1 = (p1_e1/p1_e2)/(p0_e1/p0_e2)
      or2 = (p1_e2/p1_e1)/(p0_e2/p0_e1)
      effect = ((O10*or1+1)*(O20*or2+1)-1-rho*sqrt(or1*or2*O10*O20))*(1+rho*sqrt(O10*O20))/
        (((1+O10)*(1+O20)-1-rho*sqrt(O10*O20))*(1+rho*sqrt(or1*or2*O10*O20)))
      or <- c(out_e1,out_e2,effect)
      effect_ce <- as.data.frame(or)
    }
    rownames(effect_ce) <- c("Effect E1","Effect E2", "Effect CE")
    return(effect_ce)
}
