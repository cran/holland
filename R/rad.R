#' @title degrees to radians
#' @keywords misc
#' @export rad
#' @description This function converts angular locations from degrees to radians 
#' @details no details   
#' @param x numeric values in degrees
#' @param m logical should values > 360 degrees be divided modulo 
#' @param rev logical if TRUE list objekt is returnd with number of revolutions
#' @return values in radians; optionaly number of revolutions
#' @examples 
#' ## RIASEC angular locations in degrees to radians
#' rad(c(60,120,180,240,300,360))
#' rad(720)
#' rad(720,TRUE)
#' rad(360)
#' rad(360,TRUE)
#' # 810 degrees is two full revolutions and a quater
#' # which is 1.570796 radians or 90 degrees - check it!
#' rad(810,TRUE,TRUE) 
#' 
#' 
#' 
################# funktionsbegin #########
#func. by: jhheine@googlemail.com 
rad<-function (x,m=FALSE,rev=FALSE) {
  if(rev==FALSE){
    if(m==TRUE){x=x%%360.00000000000000000000000000000000000001}
    erg<-(x * pi)/180}
  if(rev==TRUE){
    rev=x/360
    x=x%%360.00000000000000000000000000000000000001
    ra<-(x * pi)/180
    erg<-list(rev=rev, rad=ra, deg=x) 
  }
  return(erg) 
  } # ende der funktion