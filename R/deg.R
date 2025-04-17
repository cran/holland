#' @title radians to degrees 
#' @keywords misc
#' @export deg
#' @description This function converts angular locations from radians to degrees 
#' @details no details   
#' @param x numeric values in radians 
#' @param m logical should values > (2 * 3.141592653589793238462643383279) be divided modulo 
#' @param rev logical if TRUE list objekt is returnd with number of revolutions
#' @return values in degrees; optionaly number of revolutions
#' @examples 
#' ## RIASEC angular locations in radians to degrees 
#' deg(c(0, 1.047198, 2.094395, 3.141593, 4.188790, 5.235988))
#' deg(6.283185)
#' deg(6.283185,TRUE)
#' deg(12.56637)
#' deg(12.56637,TRUE)
#' # 14.137167 radians is two full revolutions and a quater
#' # which is 90 degrees or 1.570796 radians- check it!
#' deg(14.137167,TRUE,TRUE)
################# funktionsbegin #########
#func. by: jhheine@googlemail.com 
deg<-function (x,m=FALSE,rev=FALSE) {
  if(rev==FALSE){
    if(m==TRUE) {x<-x%%(2 * 3.141592653589793238462643383279)}  #6.2831861798442
    erg<-(x * 180)/3.141592653589793238462643383279}
  if(rev==TRUE){
    rev=x/(2 * 3.141592653589793238462643383279)
    x<-x%%(2 * 3.141592653589793238462643383279)
    de<-(x * 180)/3.141592653589793238462643383279
    erg<-list(rev=rev, deg=de, rad=x)
  }
  return(erg) 
  } # ende der funktion
