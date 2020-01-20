#' UPPER LIMIT FOR PEARSON CORRELATION
#'
#' @description Knowing that the correlation of a combined variable is presented in an intervalic manner,
#' and calculated from the probability of occurrence of the respective events.
#' This function allows to calculate the minimum correlation that this typology of variables can experience.
#'
#'
#' @param p_e1 numeric parameter, probability of the event E1
#' @param p_e2 numeric parameter, probability of the event E2
#'
#' @export
#'
#' @return Returns the upper correlation threshold that can be set by the compost binarius event.
#' @details upper_corr returns a numeric value negated bounded between 0 and 1.
#' The probabilities of the occurrence of events must be defined by the open interval of (0.1).
#'
upper_corr <- function(p_e1,p_e2){
  if(p_e1 < 0 || p_e1 > 1){
    stop("The probability of observing the event E1 (p_e1) must be number between 0 and 1")
  }else if(p_e2 < 0 || p_e2 > 1){
    stop("The probability of observing the event E2 (p_e2) must be number between 0 and 1")
  }else{
    upper_corr <- min(  sqrt( ( p_e1/ (1-p_e1) )/( p_e2/ (1-p_e2) ) ), sqrt((p_e2/(1-p_e2))/(p_e1/(1-p_e1)) ) )
    return(upper_corr)
  }
}
