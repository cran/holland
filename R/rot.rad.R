#' @title rotate radians by amount in radians 
#' @keywords misc
#' @export rot.rad
#' @description This function rotates angular locations in radians by an ampunt given in radians 
#' @details postive values vor \code{amount} will result in clockwise rotation an negative values will result in counterclockwise rotation     
#' @param x numeric values in radians 
#' @param amount amount to rotate (in radians) 
#' @param rev logical if TRUE list objekt is returnd with number of revolutions
#' @return values in radians rotatet by amount; optionaly number of revolutions
#' @examples 
#' ## rotation by 0.5235988 radians
#' # inital value smaler than 5.759587 radians
#' rot.rad(3.490659,0.5235988)
#' rot.rad(3.490659,0.5235988,TRUE)
#' # inital value smaler than 6.283185 radians
#' rot.rad(6.108652,0.5235988)
#' rot.rad(6.108652,0.5235988,TRUE)
#' # inital value biger than 6.283185 radians
#' rot.rad(15.70796,0.5235988)
#' rot.rad(15.70796,0.5235988,TRUE)
################# funktionsbegin #########
#func. by: jhheine@googlemail.com 
rot.rad<-function (x,amount=0,rev=FALSE) {
  if(rev==FALSE){
    erg<-(x+amount)%%(2 * 3.141592653589793238462643383279)}
  if(rev==TRUE){
    erg<-list(rotated.result=(x+amount)%%(2 * 3.141592653589793238462643383279), initial.revolutions=x/(2 * 3.141592653589793238462643383279) ,added.revolutions=amount/(2 * 3.141592653589793238462643383279))
  }
  return(erg)
}
    
    
    
    