#' ARE method for composite time to event endpoints
#' @description  
#' @export 
#' @keywords internal 
#' 
ARE_tte_array <- function(x,case,copula,rhoType){
  ARE_value <- tryCatch(ARE(rho=x,beta1=x[1], beta2=x[2], HR1=x[5], HR2=x[6], p1=x[3], p2=x[4], 
                            case = case, copula=copula, rhoType=rhoType)
                        ,error = function(e) e)
  res <- ifelse(inherits(ARE_value, "error")=="TRUE",NA,ARE_value)
  return(res)
}