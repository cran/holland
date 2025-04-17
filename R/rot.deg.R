#' @title rotate degrees by amount in degrees 
#' @keywords misc
#' @export rot.deg
#' @description This function rotates angular locations in degrees by an ampunt given in degrees 
#' @details postive values vor \code{amount} will result in clockwise rotation an negative values will result in counterclockwise rotation    
#' @param x numeric values in degrees 
#' @param amount amount to rotate 
#' @param rev logical if TRUE list objekt is returnd with number of revolutions
#' @return values in degrees rotatet by amount; optionaly number of revolutions
#' @examples 
#' ## rotation by 30 degrees
#' # inital value smaler than 330 degrees
#' rot.deg(200,30)
#' rot.deg(200,30,TRUE)
#' # inital value smaler than 360 degrees
#' rot.deg(350,30)
#' rot.deg(350,30,TRUE)
#' # inital value biger than 360 degrees
#' rot.deg(900,30)
#' rot.deg(900,30,TRUE)
################# funktionsbegin #########
#func. by: jhheine@googlemail.com 
rot.deg<-function (x,amount=0,rev=FALSE) {
  if(rev==FALSE){
    erg<-(x+amount)%%360}
  if(rev==TRUE){
    erg<-list(rotated.result=(x+amount)%%360, initial.revolutions=x/360 ,added.revolutions=amount/360)
  }
  return(erg)
}
    
    
    
    