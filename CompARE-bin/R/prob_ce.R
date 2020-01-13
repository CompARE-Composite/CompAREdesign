#' PROBABILITY OF UNION OF TWO PRIMARY EVENTS
#'
#'
#' @description This function is used to calculate the probability of the union of two events, E1 and E2,
#' knowing in advance the probability of occurrence of each of the two events by separating
#' them on the basis of the study population, and in the other band knowing
#' the Pearson coefficient that correlates the events.
#'
#'
#' @param p_e1 numeric parameter, probability of the event E1
#' @param p_e2 numeric parameter, probability of the event E2
#' @param rho numeric parameter, Pearson correlation between E1 i E2
#'
#' @export
#'
#' @return Returns the probability of a union of two events
#' @details Returns a numeric value. The input parameters representing the probability of the events
#' taking place are limited between 0 and 1, without including both values. Pearson's correlation
#' must be within the confidence interval that allows the combined variable according to the probabilities given.
#' To calculate this confidence interval you can use lower_corr and upper_corr functions that you can find in this package.
prob_ce <- function(p_e1, p_e2, rho){
  if(p_e1 < 0 || p_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p_e2 < 0 || p_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else if(rho < lower_corr(p_e1,p_e2)  ||  rho > upper_corr(p_e1,p_e2)){
    stop("The correlations of events must be in the correct interval")
  }else{
    prob_ce <- 1- (1-p_e1)*(1-p_e2)*( 1+ rho*sqrt(p_e1*p_e2/((1-p_e1)*(1-p_e2))))
    return(prob_ce)
  }
}
