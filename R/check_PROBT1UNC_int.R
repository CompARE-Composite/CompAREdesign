#' ARE method for composite time to event endpoints
#'
#' @description  
#' @export 
#' @keywords external
#'
#'
#'
check_PROBT1UNC_int <- function(PROBT1UNC_temp){
  PROBT1UNC_int_check <- tryCatch(integrate(PROBT1UNC_temp,lower=0, upper=1,subdivisions=10000)$value, error = function(e) e)
  lower_PROBT1UNC_int <- 0
  inc_lower <- 0
  while(inherits(PROBT1UNC_int_check, "error")=="TRUE"){
    lower_PROBT1UNC_int <- 0.001
    inc_lower <- inc_lower+0.001
    
    aux21 <- function(t,y) theta*exp(-theta*(ST10(t)+y))*(1-exp(-theta))/(exp(-theta)-exp(-theta*ST10(t))-exp(-theta*y)+exp(-theta*(ST10(t)+y)))^2
    aux22 <- function(u) integrate(aux21,0.001, ST20(u),t=u,subdivisions=10000)$value 
    
    lambdaC10 <- function(t) aux22(t)*fT10(t)/Sstar0(t)
    lambdaC11 <- function(t) HR1*lambdaC10(t)
    
    aux23 <- function(x,t) theta*exp(-theta*(x+ST20(t)))*(1-exp(-theta))/(exp(-theta)-exp(-theta*x)-exp(-theta*ST20(t))+exp(-theta*(x+ST20(t))))^2
    aux24 <- function(u) integrate(aux23,0.001,ST10(u),t=u,subdivisions=10000)$value
    
    lambdaC20 <- function(t) aux24(t)*fT20(t)/Sstar0(t)
    lambdaC21 <- function(t) HR2*lambdaC20(t)
    
    LambdaC20 <- function(t) integrate(lambdaC20,lower=lower_LambdaC20 + inc_lower,upper=t,subdivisions=10000)$value
    
    Lstar0 <- function(t) lambdaC10(t) + lambdaC20(t)
    Lstar1 <- function(t) lambdaC11(t) + lambdaC21(t)
    
    HRstar <- function(t) Lstar1(t)/Lstar0(t)
    logHRstar <- function(t) log(Lstar1(t)/Lstar0(t))
    temp3 <- Vectorize(function(t) logHRstar(t)*fstar0(t))
    temp4 <- integrate(temp3,lower_temp4 + inc_lower,1,subdivisions=10000)$value
    numerator <- (temp4)^2
    
    PROBT1UNC_temp_num <- function(t) exp(-HR2*LambdaC20(t))*Sstar0(t)*lambdaC10(t)
    PROBT1UNC_temp_den <- function(t) exp(-LambdaC20(t))*1/2 + exp(-HR2*LambdaC20(t))*1/2
    PROBT1UNC_temp <- Vectorize(function(t) PROBT1UNC_temp_num(t)/PROBT1UNC_temp_den(t))
    
    PROBT1UNC_int_check <- tryCatch(integrate(PROBT1UNC_temp,lower=0.001, upper=1,subdivisions=10000)$value, error = function(e) e)
  }
  
  return(lower_PROBT1UNC_int)
}